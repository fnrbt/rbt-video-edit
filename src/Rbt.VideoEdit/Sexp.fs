namespace Rbt.VideoEdit

open System
open System.Text
open System.Globalization
open System.Collections.Generic
open Rbt.Renderer
open Rbt.Sexp

/// S-expression front-end: parse a `.sexp` document into a `Clip` (+ RenderConfig). The AST is
/// an expression tree, so sexp is its native notation. Structure is parens (fail-loud), names
/// resolve against the registries (effects/transitions/components), templates are parameterized
/// subtrees expanded by substitution ($param), and footage/audio reference an assets table.
module Sexp =

    /// Parse every top-level form (Rbt.Sexp reader).
    let parseAll (input: string) : Sx list = Rbt.Sexp.Sexp.parseAll input

    // ── document state ──
    type AssetSpec = { Path: string; From: float; Len: float option }
    type Ctx = { Env: Map<string, Sx>; Assets: Map<string, AssetSpec>; DefaultDur: float<s>; W: int; H: int; Fps: int }
    // template name -> (paramNames, defaults, body)
    let private templates = Dictionary<string, string list * Map<string, Sx> * Sx>()

    let private clipKinds =
        set [ "concat"; "layer"; "seq"; "between"; "trim"; "shift"; "speed"; "place"; "mask"
              "reframe"; "video"; "solidBg"; "gradient"; "blank"; "music"; "sfx"; "component"; "inst"; "group"; "->" ]

    let private splitArgs (items: Sx list) : Sx list * Map<string, Sx> * Sx list =
        let mutable pos = []
        let mutable kw = Map.empty
        let mutable kids = []
        let rec go items =
            match items with
            | [] -> ()
            | SAtom k :: v :: rest when k.StartsWith ":" -> kw <- Map.add (k.Substring 1) v kw; go rest
            | [ SAtom k ] when k.StartsWith ":" -> ()
            | (SList (SAtom h :: _) as node) :: rest when clipKinds.Contains h || templates.ContainsKey h ->
                kids <- kids @ [ node ]; go rest
            | other :: rest -> pos <- pos @ [ other ]; go rest
        go items
        (pos, kw, kids)

    // ── value readers (resolve $vars from the template env) ──
    let rec private deref (ctx: Ctx) (sx: Sx) : Sx =
        match sx with
        | SAtom a when a.StartsWith "$" ->
            match Map.tryFind (a.Substring 1) ctx.Env with
            | Some s -> deref ctx s
            | None -> failwithf "sexp: unbound $%s" (a.Substring 1)
        | _ -> sx
    let private atomStr ctx sx = match deref ctx sx with SAtom a -> a | SStr s -> s | _ -> failwithf "sexp: expected atom, got %A" sx
    let private vsym = atomStr
    let private vstr = atomStr
    let private num (s: string) = Double.Parse(s, CultureInfo.InvariantCulture)
    let private vsec ctx sx = let a = atomStr ctx sx in num (if a.EndsWith "s" then a.Substring(0, a.Length - 1) else a)
    let private vdur ctx sx : float<s> = vsec ctx sx * 1.0<s>
    let private vfloat ctx sx = let a = atomStr ctx sx in num (if a.EndsWith "s" then a.Substring(0, a.Length - 1) else a)
    let private vint ctx sx = int (vfloat ctx sx)
    let private vbool ctx sx = match atomStr ctx sx with "true" -> true | _ -> false
    let private vcolor ctx sx =
        let s = (atomStr ctx sx).TrimStart('#')
        let b i = Convert.ToByte(s.Substring(i, 2), 16)
        if s.Length >= 8 then Color.rgba (b 0) (b 2) (b 4) (b 6)
        elif s.Length >= 6 then Color.rgb (b 0) (b 2) (b 4)
        else Color.rgb 255uy 255uy 255uy
    /// auto-detect a flat Param from a value sexp (for component/effect params)
    let private toParam ctx sx : Param =
        match deref ctx sx with
        | SStr s -> PStr s
        | SAtom a when a.StartsWith "#" -> PColor(vcolor ctx (SAtom a))
        | SAtom "true" -> PBool true
        | SAtom "false" -> PBool false
        | SAtom a ->
            match Double.TryParse((if a.EndsWith "s" then a.Substring(0, a.Length - 1) else a), NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, v -> PFloat v
            | _ -> PStr a
        | other -> PStr(sprintf "%A" other)
    let private kwParams ctx (kw: Map<string, Sx>) (drop: string list) : Params =
        kw |> Map.filter (fun k _ -> not (List.contains k drop)) |> Map.map (fun _ v -> toParam ctx v)

    let private parseBlend (s: string) : BlendMode =
        match s.ToLowerInvariant() with
        | "multiply" -> Multiply | "screen" -> Screen | "overlay" -> Overlay
        | "lighten" | "add" -> Lighten | "darken" -> Darken
        | "colordodge" | "dodge" -> ColorDodge | "colorburn" | "burn" -> ColorBurn
        | "difference" | "diff" -> Difference | "exclusion" -> Exclusion
        | "softlight" -> SoftLight | "hue" -> Hue | "saturation" -> Saturation | "luminosity" -> Luminosity
        | _ -> Normal

    // ── build ──
    let rec build (ctx: Ctx) (sx: Sx) : Clip =
        match sx with
        | SList (SAtom head :: tail) ->
            let pos, kw, kids = splitArgs tail
            let kwv k = Map.tryFind k kw
            let child0 () = build ctx (List.head kids)
            match head with
            | "concat" -> Concat(kids |> List.map (build ctx))
            | "layer" ->
                match kids with
                | [] -> Blank 0.0<s>
                | b :: overs ->
                    let baseClip = build ctx b
                    let ctx' = { ctx with DefaultDur = duration baseClip }
                    List.fold (fun acc o -> Over(acc, build ctx' o)) baseClip overs
            | "seq" ->
                let clips = kids |> List.map (build ctx)
                match kwv "gap" with
                | Some g -> let mk = buildTransition ctx g in (match clips with [] -> Blank 0.0<s> | h :: t -> List.fold mk h t)
                | None -> Concat clips
            | "between" -> let mk = buildTransition ctx (List.head pos) in mk (build ctx kids.[0]) (build ctx kids.[1])
            | "trim" -> Trim(child0 (), vdur ctx (kwv "start" |> Option.get), vdur ctx (kwv "len" |> Option.get))
            | "shift" -> Shift(child0 (), vdur ctx (kwv "by" |> Option.get))
            | "speed" -> Speed(child0 (), vfloat ctx pos.[0])
            | "ramp" -> TimeMap(child0 (), Ramp(vfloat ctx pos.[0], vfloat ctx pos.[1]))
            | "freeze" -> TimeMap(child0 (), Freeze(vdur ctx pos.[0], vdur ctx pos.[1]))
            | "reverse" -> TimeMap(child0 (), Reverse)
            | "blend" -> Blended(parseBlend (vsym ctx pos.[0]), child0 ())
            | "screen" -> Blended(Screen, child0 ())
            | "multiply" -> Blended(Multiply, child0 ())
            | "overlay" -> Blended(Overlay, child0 ())
            | "place" -> buildPlace ctx kw (child0 ())
            | "mask" -> Timeline.Mask(child0 (), buildShape ctx (match pos with s :: _ -> s | [] -> kwv "shape" |> Option.get))
            | "reframe" -> Reframe(child0 (), buildRect ctx (match pos with s :: _ -> s | [] -> kwv "rect" |> Option.get))
            | "blank" -> Blank(vdur ctx pos.[0])
            | "solidBg" -> SolidBg(vdur ctx pos.[0], vcolor ctx (if pos.Length > 1 then pos.[1] else kwv "color" |> Option.get))
            | "gradient" -> GradientBg(vdur ctx pos.[0], buildStops ctx pos.[1])
            | "video" -> buildVideo ctx pos kw
            | "music" -> buildMusic ctx pos kw
            | "sfx" -> let p = resolveAsset ctx pos.[0] in audioClip p.Path p.From (kwv "len" |> Option.map (vdur ctx) |> Option.defaultValue 1.0<s>)
            | "component" ->
                let name = vsym ctx pos.[0]
                let d = kwv "dur" |> Option.map (vdur ctx) |> Option.defaultValue ctx.DefaultDur
                Timeline.Component(d, { Name = name; Params = kwParams ctx kw [ "dur" ] })
            | "inst" -> expand ctx (vsym ctx pos.[0]) kw
            | "group" ->
                let name = match pos with p :: _ -> vstr ctx p | [] -> (kwv "name" |> Option.map (vstr ctx) |> Option.defaultValue "group")
                let enabled = kwv "enabled" |> Option.map (vbool ctx) |> Option.defaultValue true
                Timeline.Section(name, enabled, child0 ())
            | "->" -> buildPipe ctx tail   // raw tail: first = clip, rest = effects (not clip-kinds → not in `kids`)
            | name when templates.ContainsKey name -> expand ctx name kw
            | other -> failwithf "sexp: unknown node '%s'" other
        | other -> failwithf "sexp: expected (node ...), got %A" other

    and buildPipe ctx kids : Clip =
        match kids with
        | [] -> Blank 0.0<s>
        | clip :: effs ->
            effs |> List.fold (fun acc e ->
                match deref ctx e with
                | SList (SAtom name :: eargs) -> let _, ekw, _ = splitArgs eargs in Apply({ Name = name; Params = kwParams ctx ekw [] }, acc)
                | SAtom name -> Apply({ Name = name; Params = Map.empty }, acc)
                | _ -> acc) (build ctx clip)

    and buildTransition ctx sx : (Clip -> Clip -> Clip) =
        match deref ctx sx with
        | SList (SAtom name :: targs) ->
            let pos, kw, _ = splitArgs targs
            let dur = if pos.IsEmpty then 0.0<s> else vdur ctx pos.[0]
            let ps = kwParams ctx kw []
            fun a b -> Between({ Name = name; Dur = dur; Params = ps }, a, b)
        | SAtom "cut" -> fun a b -> Between({ Name = "cut"; Dur = 0.0<s>; Params = Map.empty }, a, b)
        | _ -> fun a b -> Between({ Name = "crossfade"; Dur = 0.5<s>; Params = Map.empty }, a, b)

    and buildAnim ctx sx : Anim<float> =
        match deref ctx sx with
        | SList (SAtom "tween" :: r) -> let p, _, _ = splitArgs r in A.tween (vfloat ctx p.[0]) (vfloat ctx p.[1]) (vsec ctx p.[2]) (buildEase ctx p.[3])
        | SList (SAtom "osc" :: r) -> let p, _, _ = splitArgs r in let amp = vfloat ctx p.[0] in let freq = vfloat ctx p.[1] in (fun t -> amp * sin (t * freq * 2.0 * Math.PI))
        | other -> A.constant (vfloat ctx other)

    and buildEase ctx sx : Easing =
        match deref ctx sx with
        | SAtom "linear" -> Ease.linear
        | SAtom "easeInCubic" -> Ease.easeInCubic
        | SAtom "easeOutCubic" -> Ease.easeOutCubic
        | SAtom "easeInOutCubic" -> Ease.easeInOutCubic
        | SList (SAtom "easeOutBack" :: a :: _) -> Ease.easeOutBack (vfloat ctx a)
        | SList (SAtom "spring" :: k :: d :: _) -> Ease.spring (vfloat ctx k) (vfloat ctx d)
        | _ -> Ease.easeOutCubic

    and buildShape ctx sx : (Frame -> PathSeg list) =
        match deref ctx sx with
        | SList (SAtom "circle" :: _) -> (fun f -> let r = (min f.W f.H) / 2.0 in Raster.ellipsePath (f.W / 2.0) (f.H / 2.0) r r)
        | SList (SAtom "rect" :: a) -> let p, _, _ = splitArgs a in (fun _ -> rectPath (rect (vfloat ctx p.[0]) (vfloat ctx p.[1]) (vfloat ctx p.[2]) (vfloat ctx p.[3])))
        | SList (SAtom "roundrect" :: a) -> let p, _, _ = splitArgs a in (fun _ -> Raster.roundedRectPath (rect (vfloat ctx p.[0]) (vfloat ctx p.[1]) (vfloat ctx p.[2]) (vfloat ctx p.[3])) (vfloat ctx p.[4] * 1.0<px>))
        | SList (SAtom "ellipse" :: a) -> let p, _, _ = splitArgs a in (fun _ -> Raster.ellipsePath (vfloat ctx p.[0]) (vfloat ctx p.[1]) (vfloat ctx p.[2]) (vfloat ctx p.[3]))
        | _ -> (fun f -> rectPath (rect 0.0 0.0 f.W f.H))

    and buildRect ctx sx : Rect =
        match deref ctx sx with
        | SList (SAtom "rect" :: a) -> let p, _, _ = splitArgs a in rect (vfloat ctx p.[0]) (vfloat ctx p.[1]) (vfloat ctx p.[2]) (vfloat ctx p.[3])
        | _ -> rect 0.0 0.0 100.0 100.0

    and buildPlace ctx (kw: Map<string, Sx>) (child: Clip) : Clip =
        let chan name def = match Map.tryFind name kw with Some v -> buildAnim ctx v | None -> A.constant def
        let mutable x = chan "x" 0.0
        let mutable y = chan "y" 0.0
        let mutable scale = chan "scale" 1.0
        let rot = chan "rot" 0.0
        let opacity = chan "opacity" 1.0
        // :pip (px py scl)
        match Map.tryFind "pip" kw with
        | Some (SList ps) -> let p = ps |> List.map (vfloat ctx) in x <- A.constant p.[0]; y <- A.constant p.[1]; scale <- A.constant p.[2]
        | _ -> ()
        // :enterFrom (dx dy dur)  — additive decaying offset
        match Map.tryFind "enterFrom" kw with
        | Some (SList ps) ->
            let p = ps |> List.map (atomStr ctx)
            let dx, dy, d = num p.[0], num p.[1], (let s = p.[2] in num (if s.EndsWith "s" then s.Substring(0, s.Length - 1) else s))
            let ix, iy = x, y
            x <- (fun t -> ix t + A.tween dx 0.0 d Ease.easeOutCubic t)
            y <- (fun t -> iy t + A.tween dy 0.0 d Ease.easeOutCubic t)
        | _ -> ()
        let pivot = match Map.tryFind "pivot" kw with Some (SList ps) -> let p = ps |> List.map (vfloat ctx) in Some(fun (_: Frame) -> p.[0], p.[1]) | _ -> None
        Place(child, { X = x; Y = y; Scale = scale; Rot = rot; Opacity = opacity; Pivot = pivot })

    and buildStops ctx sx : GradientStop list =
        match deref ctx sx with
        | SList items -> items |> List.mapi (fun i c -> { Offset = (if items.Length <= 1 then 0.0 else float i / float (items.Length - 1)); StopColor = vcolor ctx c })
        | _ -> []

    and resolveAsset ctx sx : AssetSpec =
        let name = vsym ctx sx
        match Map.tryFind name ctx.Assets with Some s -> s | None -> { Path = name; From = 0.0; Len = None }

    and buildVideo ctx pos (kw: Map<string, Sx>) : Clip =
        let spec = resolveAsset ctx pos.[0]
        let frm = match Map.tryFind "from" kw with Some v -> vsec ctx v | _ -> spec.From
        let len = match Map.tryFind "len" kw with Some v -> vsec ctx v | _ -> (match spec.Len with Some l -> l | None -> failwithf "sexp: video '%s' needs :len" spec.Path)
        let src = Video.loadCached spec.Path frm len ctx.W ctx.H ctx.Fps
        let clip = videoFor (len * 1.0<s>) src
        let withSnd = match Map.tryFind "sound" kw with Some v when vbool ctx v -> withSound src clip | _ -> clip
        // label footage in the timeline by its asset/file name (render-transparent)
        Timeline.Section(IO.Path.GetFileNameWithoutExtension spec.Path, true, withSnd)

    and buildMusic ctx pos (kw: Map<string, Sx>) : Clip =
        let spec = resolveAsset ctx pos.[0]
        let len = match Map.tryFind "len" kw with Some v -> vdur ctx v | _ -> (if ctx.DefaultDur > 0.0<s> then ctx.DefaultDur else 10.0<s>)
        let mutable m = audioClip spec.Path spec.From len
        match Map.tryFind "gain" kw with Some v -> m <- gain (vfloat ctx v) m | _ -> ()
        match Map.tryFind "fadeIn" kw with Some v -> m <- afadeIn (vdur ctx v) m | _ -> ()
        match Map.tryFind "fadeOut" kw with Some v -> m <- afadeOut (vdur ctx v) m | _ -> ()
        m

    // expand a template by substitution (eager): bind args into the body's $params, then build
    and expand ctx name (kw: Map<string, Sx>) : Clip =
        match templates.TryGetValue name with
        | true, (_, defaults, body) ->
            let env = Map.fold (fun acc k v -> Map.add k v acc) defaults kw   // defaults overridden by args
            // label the instance for the timeline: template name + a distinguishing param if present
            let keyParam =
                [ "title"; "name"; "label"; "headline"; "text" ]
                |> List.tryPick (fun k -> Map.tryFind k kw |> Option.map (fun v -> match v with SStr s -> s | SAtom a -> a | _ -> ""))
            let label = match keyParam with Some s when s <> "" -> sprintf "%s · %s" name s | _ -> name
            Timeline.Section(label, true, build { ctx with Env = env } body)
        | _ -> failwithf "sexp: unknown template '%s'" name

    // ── document ──
    let private parseParams (sx: Sx) : string list * Map<string, Sx> =
        match sx with
        | SList items ->
            let mutable names = []
            let mutable defs = Map.empty
            for it in items do
                match it with
                | SAtom nm -> names <- names @ [ nm ]
                | SList [ SAtom nm; d ] -> names <- names @ [ nm ]; defs <- Map.add nm d defs
                | _ -> ()
            names, defs
        | _ -> [], Map.empty

    let loadDoc (text: string) : RenderConfig * Clip =
        Fx.install ()
        Templates.install ()
        templates.Clear()
        match parseAll text with
        | [ SList (SAtom "video" :: SList dims :: rest) ] ->
            let w = match dims with a :: _ -> int (num (match a with SAtom s -> s | _ -> "1280")) | _ -> 1280
            let h = match dims with _ :: b :: _ -> int (num (match b with SAtom s -> s | _ -> "720")) | _ -> 720
            let pos, kw, _ = splitArgs rest    // section-lists land in `pos` (heads aren't clipKinds)
            let kwS k = Map.tryFind k kw |> Option.map (fun v -> match v with SStr s -> s | SAtom s -> s | _ -> "")
            let fps = kwS "fps" |> Option.map (fun s -> int (num s)) |> Option.defaultValue 30
            let bg = match Map.tryFind "bg" kw with Some v -> (let s = (match v with SAtom a -> a | _ -> "#0a0e1c").TrimStart('#') in Color.rgb (Convert.ToByte(s.Substring(0,2),16)) (Convert.ToByte(s.Substring(2,2),16)) (Convert.ToByte(s.Substring(4,2),16))) | None -> Color.rgb 10uy 14uy 28uy
            let out = kwS "out" |> Option.defaultValue "out/sexp.mp4"
            // pass 1: assets + templates/defs
            let mutable assets = Map.empty
            let sections = pos |> List.choose (function SList (SAtom h :: t) -> Some(h, t) | _ -> None)
            for (h, t) in sections do
                match h with
                | "assets" ->
                    for a in t do
                        match a with
                        | SList (SAtom nm :: SStr path :: ra) ->
                            let _, akw, _ = splitArgs ra
                            let frm = akw |> Map.tryFind "from" |> Option.map (fun v -> match v with SAtom s -> num (if s.EndsWith "s" then s.Substring(0,s.Length-1) else s) | _ -> 0.0) |> Option.defaultValue 0.0
                            let len = akw |> Map.tryFind "len" |> Option.map (fun v -> match v with SAtom s -> num (if s.EndsWith "s" then s.Substring(0,s.Length-1) else s) | _ -> 0.0)
                            assets <- Map.add nm { Path = path; From = frm; Len = len } assets
                        | _ -> ()
                | "template" ->
                    match t with SAtom nm :: ps :: body :: _ -> let names, defs = parseParams ps in templates.[nm] <- (names, defs, body) | _ -> ()
                | "def" ->
                    match t with SAtom nm :: body :: _ -> templates.[nm] <- ([], Map.empty, body) | _ -> ()
                | _ -> ()
            // pass 2: timeline
            let ctx = { Env = Map.empty; Assets = assets; DefaultDur = 0.0<s>; W = w; H = h; Fps = fps }
            let root =
                sections |> List.tryPick (fun (h, t) -> if h = "timeline" then Some(build ctx (List.head t)) else None)
                |> Option.defaultWith (fun () -> failwith "sexp: no (timeline ...) section")
            { W = w; H = h; Fps = fps; Background = bg; Output = out; Crf = 19; Preset = "medium" }, root
        | _ -> failwith "sexp: document must be a single (video (W H) ...) form"

    let renderFile (path: string) : unit =
        Theme.useBoldFont Theme.defaultBoldFontPath
        let cfg, root = loadDoc (IO.File.ReadAllText path)
        printfn "=== rendering %s → %s ===" path cfg.Output
        renderToMp4 cfg root

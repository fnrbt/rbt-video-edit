namespace VideoEdit

// NOTE: no `open Renderer` — Cmd.Mask would shadow our Timeline.Mask in the pattern matches.

/// Derives a timeline VIEW from the clip AST (a projection, not part of the composition).
/// Structural wrappers (Place/Mask/Reframe/Apply/Transform/Speed) collapse into adornment
/// badges on the item they wrap — they never get their own row. Sequences/layers/sections
/// become nested groups; leaves (footage/component/audio/bg) become bars. The widget flattens
/// this tree with a collapse-set so depth never means clutter.
module TimelineView =

    type TKind = KSeq | KLayer | KSection | KFootage | KGraphic | KAudio | KBg | KBlank | KOther

    type TItem =
        { Start: float<s>; Dur: float<s>; Label: string; Kind: TKind; Adorns: string list; Children: TItem list }

    let build (root: Clip) : TItem =
        let adorn a (it: TItem) = { it with Adorns = it.Adorns @ [ a ] }
        let leaf start label kind dur = { Start = start; Dur = dur; Label = label; Kind = kind; Adorns = []; Children = [] }
        let rec go (start: float<s>) (c: Clip) : TItem =
            match c with
            | Concat cs ->
                let mutable off = start
                let kids = [ for ch in cs do let it = go off ch in off <- off + duration ch; yield it ]
                { Start = start; Dur = off - start; Label = "sequence"; Kind = KSeq; Adorns = []; Children = kids }
            | Between (tr, _, _) ->
                // flatten a left-nested chain of same transitions into one sequence row of items
                let items = System.Collections.Generic.List<Clip>()
                let rec spine cc =
                    match cc with
                    | Between (t2, x, y) when t2.Name = tr.Name && t2.Dur = tr.Dur -> spine x; items.Add y
                    | other -> items.Add other
                spine c
                let mutable off = start
                let kids = [ for it in items do let item = go off it in off <- off + duration it - tr.Dur; yield item ]
                { Start = start; Dur = duration c; Label = "sequence"; Kind = KSeq
                  Adorns = [ sprintf "%s %.1fs" tr.Name (float tr.Dur) ]; Children = kids }
            | Over (a, b) ->
                { Start = start; Dur = duration c; Label = "layer"; Kind = KLayer; Adorns = []; Children = [ go start a; go start b ] }
            | Section (name, en, inner) ->
                let it = go start inner
                { it with Label = name; Kind = KSection; Adorns = (if en then it.Adorns else "muted" :: it.Adorns) }
            | Place (inner, _) -> adorn "place" (go start inner)
            | Mask (inner, _) -> adorn "mask" (go start inner)
            | Reframe (inner, _) -> adorn "crop" (go start inner)
            | Transform (inner, _) -> adorn "xform" (go start inner)
            | Speed (inner, f) -> adorn (sprintf "%.2gx" f) (go start inner)
            | Apply (eff, inner) -> adorn ("fx:" + eff.Name) (go start inner)
            | Trim (inner, _, l) -> { go start inner with Dur = l }
            | Shift (inner, d) -> go (start + d) inner
            | Component (d, r) -> leaf start r.Name KGraphic d
            | Inst r -> (match resolveTemplate r with Some cc -> { go start cc with Label = r.Name; Kind = KSection } | None -> leaf start r.Name KSection 0.0<s>)
            | Audio (d, a) -> leaf start (System.IO.Path.GetFileNameWithoutExtension a.Path) KAudio d
            | Graphic (d, _) -> leaf start "graphic" KGraphic d
            | SolidBg (d, _) -> leaf start "color" KBg d
            | GradientBg (d, _) -> leaf start "gradient" KBg d
            | Blank d -> leaf start "blank" KBlank d
        go 0.0<s> root

    /// A drawable row: depth (indent), a stable path id (collapse/select), and the item.
    type Row = { Depth: int; Path: string; Item: TItem; HasChildren: bool }

    /// Flatten to rows given a collapse-set of paths (root's children become the top rows).
    let flatten (collapsed: Set<string>) (root: TItem) : Row list =
        let rows = System.Collections.Generic.List<Row>()
        let rec go depth path (it: TItem) =
            let hasKids = not it.Children.IsEmpty
            rows.Add { Depth = depth; Path = path; Item = it; HasChildren = hasKids }
            if hasKids && not (Set.contains path collapsed) then
                it.Children |> List.iteri (fun i ch -> go (depth + 1) (path + "/" + string i) ch)
        // skip the root wrapper itself; show its children as the top rows
        match root.Children with
        | [] -> go 0 "0" root
        | kids -> kids |> List.iteri (fun i ch -> go 0 (string i) ch)
        List.ofSeq rows

    /// Paths that should start collapsed: anything with children at depth >= `minDepth`.
    let initialCollapsed (minDepth: int) (root: TItem) : Set<string> =
        let acc = System.Collections.Generic.HashSet<string>()
        let rec go depth path (it: TItem) =
            if depth >= minDepth && not it.Children.IsEmpty then acc.Add path |> ignore
            it.Children |> List.iteri (fun i ch -> go (depth + 1) (path + "/" + string i) ch)
        match root.Children with
        | [] -> ()
        | kids -> kids |> List.iteri (fun i ch -> go 0 (string i) ch)
        Set.ofSeq acc

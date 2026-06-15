namespace VideoEdit

open Renderer

/// The clip algebra. Structural combinators are a closed DU; effects and transitions are
/// OPEN — referenced by serializable name + params and resolved against registries at eval
/// time. `eval` lowers a clip at time t into a renderer `Cmd list`.
[<AutoOpen>]
module Timeline =

    /// Serializable reference to a registered effect / transition (identity is dynamic).
    type EffectRef = { Name: string; Params: Params }
    type TransitionRef = { Name: string; Dur: float<s>; Params: Params }

    /// Reference to a registered component / clip TEMPLATE, with flat params — the same
    /// name+params pattern as effects, lifted to graphics and whole clips. A zero-param ref is
    /// just a named reference; with params it's a template instantiation. Pure data → diffable.
    type InstRef = { Name: string; Params: Params }

    /// Audio bound into the clip tree. Because it lives INSIDE the structural combinators, the
    /// same Concat/Trim/Shift/Speed/Over that move/retime the picture move/retime the sound —
    /// track + clip travel as a unit. `In` is the source in-point; Gain/Fades/Speed are applied
    /// by the mixer (`collectAudio` → ffmpeg filtergraph).
    type AudioRef =
        { Path: string; In: float<s>; Gain: float; FadeIn: float<s>; FadeOut: float<s>; Speed: float }

    /// One flattened audio segment to mix onto the master timeline (output of `collectAudio`).
    type AudioPlacement =
        { Path: string; Start: float<s>; In: float<s>; Dur: float<s>
          Gain: float; Speed: float; FadeIn: float<s>; FadeOut: float<s> }

    /// Animated, decomposed transform applied to a WHOLE clip as a unit — the compositional
    /// spatial primitive. Channels are functions of the clip's LOCAL time (seconds); scale and
    /// rotation pivot defaults to the frame centre. Subsumes the static `Transform` and the
    /// `Scene` scenegraph's node transform, and composes hierarchically (a `Place` containing
    /// `Place`d clips → nested `Group`s → parent transform carries the children).
    type Xf =
        { X: Anim<float>; Y: Anim<float>; Scale: Anim<float>; Rot: Anim<float>
          Opacity: Anim<float>; Pivot: (Frame -> float * float) option }

    type Clip =
        // primitives
        | Blank of dur: float<s>
        | SolidBg of dur: float<s> * Color
        | GradientBg of dur: float<s> * GradientStop list        // vertical, top → bottom over the frame
        | Graphic of dur: float<s> * (Frame -> float<s> -> Cmd list)   // escape hatch / components
        | Audio of dur: float<s> * AudioRef                            // audio-only (silent picture)
        // structural combinators (closed — fundamental, not "effects")
        | Concat of Clip list                                    // A then B then …  (durations sum)
        | Over of under: Clip * over: Clip                       // composite          (dur = max)
        | Trim of Clip * start: float<s> * len: float<s>
        | Shift of Clip * delay: float<s>
        | Transform of Clip * Matrix3x2                          // static affine → Group
        | Place of Clip * Xf                                     // animated transform of a whole clip (as a unit)
        | Mask of Clip * (Frame -> PathSeg list)                 // clip output to a shape (content in place)
        | Reframe of Clip * Rect                                 // show a sub-region remapped to fill the frame
        | Speed of Clip * float                                  // dur = dur c / factor
        // open extension points (identity resolved via registry)
        | Apply of EffectRef * Clip                              // any per-clip, time-varying effect
        | Between of TransitionRef * Clip * Clip                 // any A→B transition over Dur overlap
        | Component of dur: float<s> * InstRef                   // templated graphic (registry-resolved)
        | Inst of InstRef                                        // templated clip / named reference
        | Section of name: string * enabled: bool * Clip         // named, render-transparent group (timeline unit / mute)

    /// frame -> clipDuration -> params -> localTime -> cmds-at-t -> modified cmds
    type EffectImpl = Frame -> float<s> -> Params -> float<s> -> Cmd list -> Cmd list
    /// frame -> params -> progress[0,1] -> outgoing -> incoming -> combined
    type TransitionImpl = Frame -> Params -> float -> Cmd list -> Cmd list -> Cmd list

    /// A component template: flat params → a graphic (frame, local time → cmds).
    type ComponentImpl = Params -> Frame -> float<s> -> Cmd list
    /// A clip template: flat params → a clip.
    type ClipTemplate = Params -> Clip

    let private effects = Registry<EffectImpl>()
    let private transitions = Registry<TransitionImpl>()
    let private components = Registry<ComponentImpl>()
    let private clipTemplates = Registry<ClipTemplate>()

    let registerEffect (name: string) (impl: EffectImpl) = effects.Register(name, impl)
    let registerTransition (name: string) (impl: TransitionImpl) = transitions.Register(name, impl)
    let registerComponent (name: string) (impl: ComponentImpl) = components.Register(name, impl)
    let registerClipTemplate (name: string) (impl: ClipTemplate) = clipTemplates.Register(name, impl)
    let effectNames () = effects.Names
    let transitionNames () = transitions.Names
    let componentNames () = components.Names
    let clipTemplateNames () = clipTemplates.Names
    let resolveTemplate (ref: InstRef) : Clip option = clipTemplates.TryGet ref.Name |> Option.map (fun f -> f ref.Params)

    let rec duration (c: Clip) : float<s> =
        match c with
        | Blank d | SolidBg (d, _) | GradientBg (d, _) | Graphic (d, _) | Audio (d, _) -> d
        | Concat cs -> cs |> List.sumBy duration
        | Over (a, b) -> max (duration a) (duration b)
        | Trim (_, _, len) -> len
        | Shift (c, d) -> d + duration c
        | Transform (c, _) | Apply (_, c) | Place (c, _) | Mask (c, _) | Reframe (c, _) -> duration c
        | Speed (c, f) -> duration c / f
        | Component (d, _) -> d
        | Section (_, _, c) -> duration c
        | Inst ref -> match resolveTemplate ref with Some c -> duration c | None -> 0.0<s>
        | Between (tr, a, b) -> duration a + duration b - tr.Dur

    let rec eval (frame: Frame) (c: Clip) (t: float<s>) : Cmd list =
        if t < 0.0<s> || t >= duration c then []
        else
            match c with
            | Blank _ -> []
            | SolidBg (_, color) -> [ FillRect(rect 0.0 0.0 frame.W frame.H, color) ]
            | GradientBg (_, stops) ->
                let r = rect 0.0 0.0 frame.W frame.H
                let p = LinearGradient(vec2 0.0 0.0, vec2 0.0 frame.H, stops, Pad)
                [ FillPath(rectPath r, p, NonZero) ]
            | Graphic (_, f) -> f frame t
            | Audio _ -> []                       // audio contributes no pixels
            | Concat cs ->
                let rec go acc rest =
                    match rest with
                    | [] -> []
                    | head :: tl ->
                        let d = duration head
                        if t < acc + d then eval frame head (t - acc)
                        else go (acc + d) tl
                go 0.0<s> cs
            | Over (a, b) -> eval frame a t @ eval frame b t
            | Trim (inner, start, _) -> eval frame inner (start + t)
            | Shift (inner, d) -> if t < d then [] else eval frame inner (t - d)
            | Transform (inner, m) -> [ Group(m, 1.0, eval frame inner t) ]
            | Place (inner, xf) ->
                let cmds = eval frame inner t
                if List.isEmpty cmds then []
                else
                    let tt = float t
                    let s = xf.Scale tt
                    let px, py = match xf.Pivot with Some f -> f frame | None -> frame.W / 2.0, frame.H / 2.0
                    let core = aboutPivot px py (Matrix3x2.multiply (Matrix3x2.scale s s) (Matrix3x2.rotate (xf.Rot tt)))
                    let m = Matrix3x2.multiply core (Matrix3x2.translate (xf.X tt) (xf.Y tt))
                    [ Group(m, max 0.0 (min 1.0 (xf.Opacity tt)), cmds) ]
            | Mask (inner, shape) ->
                let cmds = eval frame inner t
                if List.isEmpty cmds then [] else [ Cmd.Clip(shape frame, NonZero, cmds) ]
            | Reframe (inner, r) ->
                let cmds = eval frame inner t
                if List.isEmpty cmds then []
                else
                    let rx, ry = float r.Pos.X, float r.Pos.Y
                    let rw, rh = float r.Size.X, float r.Size.Y
                    let sx = if rw <= 0.0 then 1.0 else frame.W / rw
                    let sy = if rh <= 0.0 then 1.0 else frame.H / rh
                    [ Group(Matrix3x2.multiply (Matrix3x2.translate -rx -ry) (Matrix3x2.scale sx sy), 1.0, cmds) ]
            | Speed (inner, f) -> eval frame inner (t * f)
            | Apply (eff, inner) ->
                let cmds = eval frame inner t
                match effects.TryGet eff.Name with
                | Some impl -> impl frame (duration inner) eff.Params t cmds
                | None -> cmds
            | Between (tr, a, b) ->
                let da = duration a
                let bStart = da - tr.Dur
                if t >= bStart && t < da then
                    let p = if tr.Dur <= 0.0<s> then 1.0 else float ((t - bStart) / tr.Dur)
                    let outCmds = eval frame a t
                    let inCmds = eval frame b (t - bStart)
                    match transitions.TryGet tr.Name with
                    | Some impl -> impl frame tr.Params p outCmds inCmds
                    | None -> outCmds @ inCmds
                elif t < bStart then eval frame a t
                else eval frame b (t - bStart)
            | Component (_, ref) ->
                match components.TryGet ref.Name with
                | Some impl -> impl ref.Params frame t
                | None -> []
            | Section (_, enabled, c) -> if enabled then eval frame c t else []
            | Inst ref ->
                match resolveTemplate ref with
                | Some c -> eval frame c t
                | None -> []

    /// Flatten all audio in a clip into absolute-timeline placements, applying the SAME
    /// structural time transforms that `eval` applies to the picture. `gain`/`afadeIn`/`afadeOut`
    /// are recognised here as audio-only `Apply` effects (they are visual no-ops in `eval`).
    let collectAudio (root: Clip) : AudioPlacement list =
        let rec go (start: float<s>) (speed: float) (gain: float) (c: Clip) : AudioPlacement list =
            match c with
            | Audio (d, a) ->
                [ { Path = a.Path; Start = start; In = a.In; Dur = d / speed
                    Gain = gain * a.Gain; Speed = speed * a.Speed
                    FadeIn = a.FadeIn; FadeOut = a.FadeOut } ]
            | Blank _ | SolidBg _ | GradientBg _ | Graphic _ | Component _ -> []
            | Section (_, enabled, c) -> if enabled then go start speed gain c else []
            | Inst ref -> match resolveTemplate ref with Some c -> go start speed gain c | None -> []
            | Concat cs ->
                let mutable acc = start
                [ for ch in cs do
                    yield! go acc speed gain ch
                    acc <- acc + duration ch ]
            | Over (a, b) -> go start speed gain a @ go start speed gain b
            | Shift (inner, d) -> go (start + d) speed gain inner
            | Transform (inner, _) | Place (inner, _) | Mask (inner, _) | Reframe (inner, _) -> go start speed gain inner
            | Speed (inner, f) -> go start (speed * f) gain inner
            | Apply (eff, inner) ->
                match eff.Name with
                | "gain" -> go start speed (gain * pfloat eff.Params "g" 1.0) inner
                | "afadeIn" -> go start speed gain inner |> List.map (fun p -> { p with FadeIn = pfloat eff.Params "d" 0.0 * 1.0<s> })
                | "afadeOut" -> go start speed gain inner |> List.map (fun p -> { p with FadeOut = pfloat eff.Params "d" 0.0 * 1.0<s> })
                | _ -> go start speed gain inner
            | Trim (inner, s, len) ->
                // window inner-local [s, s+len] → output [start, start+len]
                let winA, winB = start + s, start + s + len
                go start speed gain inner
                |> List.choose (fun p ->
                    let segA = max p.Start winA
                    let segB = min (p.Start + p.Dur) winB
                    if segB <= segA then None
                    else Some { p with
                                  Start = start + (segA - winA)
                                  In = p.In + (segA - p.Start) * p.Speed
                                  Dur = segB - segA })
            | Between (tr, a, b) ->
                let bStart = start + duration a - tr.Dur
                go start speed gain a @ go bStart speed gain b
        go 0.0<s> 1.0 1.0 root

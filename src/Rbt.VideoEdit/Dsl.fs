namespace Rbt.VideoEdit

open Rbt.Renderer

/// The authoring surface: thin smart-constructors and operators over the clip algebra.
/// Effects/transitions are named registry entries (see Fx.fs); the generic `effect` /
/// `transition` helpers reach any registered one, and there are named wrappers for the common.
[<AutoOpen>]
module Dsl =

    // ── primitives ──
    let blank (d: float<s>) = Blank d
    let solidBg (d: float<s>) (color: Color) = SolidBg(d, color)
    let gradientBg (d: float<s>) (stops: GradientStop list) = GradientBg(d, stops)
    let graphic (d: float<s>) (content: Frame -> float<s> -> Cmd list) = Graphic(d, content)

    // ── templated graphics / clips (name + flat params, registry-resolved → pure data, diffable) ──
    /// A templated graphic: `comp "lowerThird" [ "name", PStr "ALEX"; … ] 3.0<s>`.
    let comp (name: string) (ps: (string * Param) list) (d: float<s>) : Clip = Timeline.Component(d, { Name = name; Params = pmap ps })
    /// Instantiate a registered clip template with params.
    let inst (name: string) (ps: (string * Param) list) : Clip = Timeline.Inst { Name = name; Params = pmap ps }
    /// A plain named reference (zero-param template instance).
    let ref' (name: string) : Clip = Timeline.Inst { Name = name; Params = Map.empty }

    /// A footage clip (full length of the decoded source), drawn full-frame.
    let video (src: VideoSource) : Clip =
        Graphic(Video.duration src, fun frame t ->
            let fb = Video.frameAt src t
            [ DrawImage(fb, rect 0.0 0.0 (float src.W) (float src.H), rect 0.0 0.0 frame.W frame.H, true) ])

    /// A footage clip limited to `dur` (freezes on the last frame if the source is shorter).
    let videoFor (dur: float<s>) (src: VideoSource) : Clip =
        Graphic(dur, fun frame t ->
            let fb = Video.frameAt src t
            [ DrawImage(fb, rect 0.0 0.0 (float src.W) (float src.H), rect 0.0 0.0 frame.W frame.H, true) ])

    /// A footage clip backed by a STREAMING source (bounded memory; decodes on demand). Use for
    /// long / high-resolution footage that would be too big to decode fully into RAM.
    let videoStream (src: Video.StreamSource) : Clip =
        Graphic(src.Duration, fun frame t ->
            let fb = src.FrameAt t
            [ DrawImage(fb, rect 0.0 0.0 (float src.W) (float src.H), rect 0.0 0.0 frame.W frame.H, true) ])
    /// Streaming footage WITH its own audio bound (moves/retimes as a unit).
    let videoStreamSnd (src: Video.StreamSource) : Clip =
        Over(videoStream src, Audio(src.Duration, { Path = src.Path; In = src.Start; Gain = 1.0; FadeIn = 0.0<s>; FadeOut = 0.0<s>; Speed = 1.0 }))

    // ── audio ──
    /// Audio-only clip from a file: source in-point `inSec`, timeline length `dur` (silent picture).
    let audioClip (path: string) (inSec: float) (dur: float<s>) : Clip =
        Audio(dur, { Path = path; In = inSec * 1.0<s>; Gain = 1.0; FadeIn = 0.0<s>; FadeOut = 0.0<s>; Speed = 1.0 })
    let music (path: string) (dur: float<s>) = audioClip path 0.0 dur
    let sfx (path: string) (dur: float<s>) = audioClip path 0.0 dur
    /// Attach a footage source's own audio to a clip, bound to move/retime with it.
    let withSound (src: VideoSource) (c: Clip) : Clip =
        Over(c, Audio(duration c, { Path = src.Path; In = src.Start; Gain = 1.0; FadeIn = 0.0<s>; FadeOut = 0.0<s>; Speed = 1.0 }))
    /// Footage drawn full-frame WITH its own audio.
    let videoSnd (src: VideoSource) : Clip = withSound src (video src)
    // audio-only adjustments — visual no-ops in eval, interpreted by collectAudio
    let gain (g: float) c = Apply({ Name = "gain"; Params = pmap [ "g", PFloat g ] }, c)
    let afadeIn (d: float<s>) c = Apply({ Name = "afadeIn"; Params = pmap [ "d", PFloat (float d) ] }, c)
    let afadeOut (d: float<s>) c = Apply({ Name = "afadeOut"; Params = pmap [ "d", PFloat (float d) ] }, c)

    // ── structural combinators ──
    let concat (cs: Clip list) = Concat cs
    let (++) (a: Clip) (b: Clip) = Concat [ a; b ]
    let over (under: Clip) (top: Clip) = Over(under, top)
    let (.>) (under: Clip) (top: Clip) = Over(under, top)
    /// Stack many overlays on top of a base, in order.
    let layer (base': Clip) (overlays: Clip list) = List.fold (fun acc o -> Over(acc, o)) base' overlays
    let trim (start: float<s>) (len: float<s>) (c: Clip) = Trim(c, start, len)
    let shift (d: float<s>) (c: Clip) = Shift(c, d)
    let xform (m: Matrix3x2) (c: Clip) = Transform(c, m)
    let speed (f: float) (c: Clip) = Speed(c, f)

    // ── blend modes: composite a layer onto what is UNDER it with a Porter-Duff / separable mode.
    //    `over base (screen glow)` screens the glow onto the base. Wraps any clip. ──
    let blend (mode: BlendMode) (c: Clip) : Clip = Blended(mode, c)
    let screen c = blend Screen c
    let multiply c = blend Multiply c
    let overlay c = blend Overlay c
    let lighten c = blend Lighten c
    let darken c = blend Darken c
    let colorDodge c = blend ColorDodge c          // additive-style glow
    let difference c = blend Difference c

    // ── time remapping (generalises constant `speed`): ramps, freeze-frames, reverse ──
    /// Ease playback speed from `s0`× to `s1`× across the clip (output duration derived).
    let timeRamp (s0: float) (s1: float) (c: Clip) : Clip = TimeMap(c, Ramp(s0, s1))
    let rampUp (s: float) c = timeRamp 1.0 s c
    let rampDown (s: float) c = timeRamp s 1.0 c
    /// Hold a single source frame (at source time `at`) for `hold` seconds — a freeze frame.
    let freezeAt (at: float<s>) (hold: float<s>) (c: Clip) : Clip = TimeMap(c, Freeze(at, hold))
    /// Play the clip backwards.
    let reverse (c: Clip) : Clip = TimeMap(c, Reverse)
    /// Arbitrary time remap: `dur` output, mapping output-local → source-local time.
    let timeMapF (dur: float<s>) (f: float<s> -> float<s>) (c: Clip) : Clip = TimeMap(c, Remap(dur, f))
    /// Chain clips with a per-gap transition builder, e.g. `chain (crossfade 0.5<s>) [a;b;c]`.
    let chain (mk: Clip -> Clip -> Clip) (clips: Clip list) : Clip =
        match clips with
        | [] -> Blank 0.0<s>
        | h :: t -> List.fold mk h t

    // ── animated placement: transform a WHOLE clip as a unit (position/scale/rotation/opacity).
    //    Composes with everything — nest, sequence, transition — and bound audio rides along.
    //    Chained calls merge into a single Place; set base position first, then layer entrances. ──
    let private idXf : Xf =
        { X = A.constant 0.0; Y = A.constant 0.0; Scale = A.constant 1.0
          Rot = A.constant 0.0; Opacity = A.constant 1.0; Pivot = None }
    let private withXf (f: Xf -> Xf) (c: Clip) : Clip =
        match c with
        | Place (inner, xf) -> Place(inner, f xf)
        | _ -> Place(c, f idXf)
    let private plusA (a: Anim<float>) (b: Anim<float>) : Anim<float> = fun t -> a t + b t

    // constant transforms
    let place (px: float) (py: float) c = withXf (fun x -> { x with X = A.constant px; Y = A.constant py }) c
    let scale (v: float) c = withXf (fun x -> { x with Scale = A.constant v }) c
    let rotate (rad: float) c = withXf (fun x -> { x with Rot = A.constant rad }) c
    let opacity (v: float) c = withXf (fun x -> { x with Opacity = A.constant v }) c
    let pivot (px: float) (py: float) c = withXf (fun x -> { x with Pivot = Some(fun _ -> px, py) }) c
    /// Picture-in-picture: shrink the whole clip to `scl` and offset (px,py) from the centre.
    let pip (px: float) (py: float) (scl: float) c = c |> scale scl |> place px py
    // animated channels (functions of local time, seconds)
    let animPos (x: Anim<float>) (y: Anim<float>) c = withXf (fun xf -> { xf with X = x; Y = y }) c
    let animScale (a: Anim<float>) c = withXf (fun xf -> { xf with Scale = a }) c
    let animRot (a: Anim<float>) c = withXf (fun xf -> { xf with Rot = a }) c
    let animOpacity (a: Anim<float>) c = withXf (fun xf -> { xf with Opacity = a }) c
    // entrance presets (translation is additive, so it composes with `place`)
    let enterFrom (dx: float) (dy: float) (d: float<s>) c =
        withXf (fun x -> { x with X = plusA x.X (A.tween dx 0.0 (float d) Ease.easeOutCubic)
                                  Y = plusA x.Y (A.tween dy 0.0 (float d) Ease.easeOutCubic) }) c
    let popIn (d: float<s>) c =
        withXf (fun x -> { x with Scale = A.tween 0.7 1.0 (float d) (Ease.easeOutBack 2.0)
                                  Opacity = A.tween 0.0 1.0 (float d * 0.5) Ease.easeOutCubic }) c
    let appear (d: float<s>) c = withXf (fun x -> { x with Opacity = A.tween 0.0 1.0 (float d) Ease.easeOutCubic }) c

    // ── 2.5D: animated 3D / perspective transform of a whole clip (renderer Group3D).
    //    Parallax, card flips, perspective lower-thirds. Chained calls merge into one Place3D. ──
    let private pi = System.Math.PI
    let private idXf3 : Xf3 =
        { X = A.constant 0.0; Y = A.constant 0.0; Z = A.constant 0.0
          RotX = A.constant 0.0; RotY = A.constant 0.0; RotZ = A.constant 0.0
          Scale = A.constant 1.0; Opacity = A.constant 1.0; Persp = 1200.0; Pivot = None }
    let private withXf3 (f: Xf3 -> Xf3) (c: Clip) : Clip =
        match c with Place3D (inner, xf) -> Place3D(inner, f xf) | _ -> Place3D(c, f idXf3)
    /// Perspective distance in px (smaller = stronger foreshortening).
    let perspectiveDist (d: float) c = withXf3 (fun x -> { x with Persp = d }) c
    let pivot3 (px: float) (py: float) c = withXf3 (fun x -> { x with Pivot = Some(fun _ -> px, py) }) c
    // constant pose
    let tiltX (rad: float) c = withXf3 (fun x -> { x with RotX = A.constant rad }) c
    let tiltY (rad: float) c = withXf3 (fun x -> { x with RotY = A.constant rad }) c
    let tiltZ (rad: float) c = withXf3 (fun x -> { x with RotZ = A.constant rad }) c
    let depth (z: float) c = withXf3 (fun x -> { x with Z = A.constant z }) c
    let scale3 (v: float) c = withXf3 (fun x -> { x with Scale = A.constant v }) c
    let place3 (px: float) (py: float) c = withXf3 (fun x -> { x with X = A.constant px; Y = A.constant py }) c
    // animated channels (local time, seconds)
    let animRotX (a: Anim<float>) c = withXf3 (fun x -> { x with RotX = a }) c
    let animRotY (a: Anim<float>) c = withXf3 (fun x -> { x with RotY = a }) c
    let animRotZ (a: Anim<float>) c = withXf3 (fun x -> { x with RotZ = a }) c
    let animZ (a: Anim<float>) c = withXf3 (fun x -> { x with Z = a }) c
    // presets
    /// Flip in around the Y axis (edge-on → facing) over d, with a quick fade.
    let flipInY (d: float<s>) c =
        withXf3 (fun x -> { x with RotY = A.tween (pi / 2.0) 0.0 (float d) Ease.easeOutCubic
                                   Opacity = A.tween 0.0 1.0 (float d * 0.4) Ease.easeOutCubic }) c
    /// Continuous gentle Y-axis swivel: amplitude rad at freq Hz.
    let swivelY (amp: float) (freq: float) c = withXf3 (fun x -> { x with RotY = fun t -> amp * sin (t * freq * 2.0 * pi) }) c

    // ── crop / mask: clip-native region + shape (makes PiP / facecam fully compositional) ──
    /// Clip a clip's output to a shape (content stays in place). Shape is frame-aware.
    let mask (shape: Frame -> PathSeg list) (c: Clip) : Clip = Timeline.Mask(c, shape)
    /// Rectangular crop (absolute coords) — hide everything outside the rect.
    let crop (r: Rect) (c: Clip) : Clip = mask (fun _ -> rectPath r) c
    /// Rounded-rect mask (absolute coords, corner radius px).
    let maskRound (r: Rect) (radius: float) (c: Clip) : Clip =
        mask (fun _ -> Raster.roundedRectPath r (radius * 1.0<px>)) c
    /// Ellipse mask (absolute centre + radii).
    let maskEllipse (cx: float) (cy: float) (rx: float) (ry: float) (c: Clip) : Clip =
        mask (fun _ -> Raster.ellipsePath cx cy rx ry) c
    /// Centred circular mask filling the frame's smaller dimension (facecam).
    let circleMask (c: Clip) : Clip =
        mask (fun f -> let r = (min f.W f.H) / 2.0 in Raster.ellipsePath (f.W / 2.0) (f.H / 2.0) r r) c
    /// Show a sub-region remapped to fill the frame (punch-in / crop a region out of the source).
    let reframe (r: Rect) (c: Clip) : Clip = Timeline.Reframe(c, r)

    // ── effects (generic + named wrappers) ──
    let effect (name: string) (ps: (string * Param) list) (c: Clip) = Apply({ Name = name; Params = pmap ps }, c)
    let fadeIn (d: float<s>) c = effect "fadeIn" [ "dur", PFloat (float d) ] c
    let fadeOut (d: float<s>) c = effect "fadeOut" [ "dur", PFloat (float d) ] c
    let fadeInOut (din: float<s>) (dout: float<s>) c = effect "fade" [ "in", PFloat (float din); "out", PFloat (float dout) ] c
    let zoom (from: float) (to': float) c = effect "zoom" [ "from", PFloat from; "to", PFloat to' ] c
    let kenBurns c = effect "kenBurns" [] c
    let pulse c = effect "pulse" [] c
    let shake (amp: float) c = effect "shake" [ "amp", PFloat amp ] c
    let spinFx (turns: float) c = effect "spin" [ "turns", PFloat turns ] c
    let wobble c = effect "wobble" [] c
    let floatY c = effect "floatY" [] c
    let riseIn c = effect "riseIn" [] c
    let scaleIn c = effect "scaleIn" [] c
    let slideInFx (dir: string) (d: float<s>) c = effect "slideIn" [ "dir", PStr dir; "dur", PFloat (float d) ] c
    let blurFx (r: int) c = effect "blur" [ "radius", PInt r ] c
    let grayscale c = effect "grayscale" [] c
    let sepia c = effect "sepia" [] c
    let saturate (amt: float) c = effect "saturate" [ "amount", PFloat amt ] c
    let brightness (amt: float) c = effect "brightness" [ "amount", PFloat amt ] c
    let contrast (amt: float) c = effect "contrast" [ "amount", PFloat amt ] c
    let hueShift c = effect "hueRotate" [] c
    let tint (col: Color) (amt: float) c = effect "tint" [ "color", PColor col; "amount", PFloat amt ] c
    let vignette c = effect "vignette" [] c
    let flash c = effect "flash" [] c
    let glow c = effect "glow" [] c

    // ── transitions (generic + named wrappers) ──
    let transition (name: string) (d: float<s>) (ps: (string * Param) list) (a: Clip) (b: Clip) =
        Between({ Name = name; Dur = d; Params = pmap ps }, a, b)
    let crossfade (d: float<s>) a b = transition "crossfade" d [] a b
    let dissolve (d: float<s>) a b = transition "dissolve" d [] a b
    let cut a b = transition "cut" 0.0<s> [] a b
    let slide (dir: string) (d: float<s>) a b = transition "slide" d [ "dir", PStr dir ] a b
    let push (dir: string) (d: float<s>) a b = transition "push" d [ "dir", PStr dir ] a b
    let wipe (dir: string) (d: float<s>) a b = transition "wipe" d [ "dir", PStr dir ] a b
    let iris (d: float<s>) a b = transition "iris" d [] a b
    let irisClose (d: float<s>) a b = transition "irisClose" d [] a b
    let dipTo (col: Color) (d: float<s>) a b = transition "dipColor" d [ "color", PColor col ] a b
    let dipBlack (d: float<s>) a b = dipTo Color.black d a b
    let dipWhite (d: float<s>) a b = dipTo Color.white d a b
    let zoomTrans (d: float<s>) a b = transition "zoomIn" d [] a b
    let zoomOutTrans (d: float<s>) a b = transition "zoomOut" d [] a b
    let spinTrans (d: float<s>) a b = transition "spin" d [] a b
    let whip (dir: string) (d: float<s>) a b = transition "whip" d [ "dir", PStr dir ] a b
    let scaleFade (d: float<s>) a b = transition "scaleFade" d [] a b

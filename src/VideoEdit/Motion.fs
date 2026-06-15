namespace VideoEdit

open Renderer

/// Motion systems — pure, deterministic, time-driven generators of `Cmd list`:
///   • kinetic typography (per-glyph staggered entrances / continuous motion),
///   • text-on-path (glyphs laid along a parametric curve),
///   • motion-along-curve (Bézier position/tangent for moving anything),
///   • a seeded particle/emitter system.
/// Everything is a function of local time `t` (no per-frame state), so it is reproducible and
/// fps-independent — wrap any of these in `graphic dur (fun frame t -> …)` to get a clip.
[<AutoOpen>]
module Motion =

    let private clamp01 (x: float) = max 0.0 (min 1.0 x)

    // ── kinetic typography ──────────────────────────────────────────────────────────────────
    // Each glyph gets its own animated transform. `perGlyph i n t` returns (dx, dy, scale, rot, opacity)
    // for glyph `i` of `n` at local time `t` — animate entrances, waves, explosions, etc.
    type GlyphAnim = int -> int -> float<s> -> float * float * float * float * float

    /// Lay out `text` from (x0, baseY) and draw each glyph through `anim`. Per-glyph layout uses
    /// each character's own advance (robust; no cross-glyph kerning, which display text rarely needs).
    let kineticText (font: Text.FontData) (size: float) (paint: Paint) (x0: float) (baseY: float)
                    (anim: GlyphAnim) (text: string) (t: float<s>) : Cmd list =
        let n = text.Length
        let mutable penX = x0
        [ for i in 0 .. n - 1 do
            let ch = string text.[i]
            let w = Text.measureWidth font size ch
            let dx, dy, sc, rot, op = anim i n t
            if op > 0.001 && sc > 0.0001 then
                let cx, cy = penX + w / 2.0, baseY - size * 0.32        // glyph centre (pivot)
                let core = aboutPivot cx cy (Matrix3x2.multiply (Matrix3x2.scale sc sc) (Matrix3x2.rotate rot))
                let m = Matrix3x2.multiply core (Matrix3x2.translate dx dy)
                yield Group(m, clamp01 op, [ DrawGlyphs(vec2 penX baseY, Text.layout font size ch, paint) ])
            penX <- penX + w ]

    /// Glyph i starts after i*gap, rises `rise` px into place and fades in over `dur` (easeOutCubic).
    let riseStagger (gap: float<s>) (dur: float<s>) (rise: float) : GlyphAnim =
        fun i _ t ->
            let lt = float (t - float i * gap)
            let u = clamp01 (lt / float dur)
            let e = Ease.eval Ease.easeOutCubic u
            (0.0, rise * (1.0 - e), 1.0, 0.0, clamp01 (lt / (float dur * 0.6)))

    /// Glyph i pops in (scale 0→1 with overshoot) + fades, staggered by i*gap.
    let popStagger (gap: float<s>) (dur: float<s>) : GlyphAnim =
        fun i _ t ->
            let lt = float (t - float i * gap)
            let u = clamp01 (lt / float dur)
            (0.0, 0.0, Ease.eval (Ease.easeOutBack 2.4) u, 0.0, clamp01 (lt / (float dur * 0.5)))

    /// Continuous per-glyph vertical wave: amplitude px at freq Hz, phase-shifted by glyph index.
    let waveStagger (amp: float) (freq: float) (phasePerGlyph: float) : GlyphAnim =
        fun i _ t -> (0.0, amp * sin (float t * freq * 2.0 * System.Math.PI + float i * phasePerGlyph), 1.0, 0.0, 1.0)

    // ── text on a path ──────────────────────────────────────────────────────────────────────
    /// A path parameterised by arc-distance: distance → (point, tangent-angle radians).
    type PathFn = float -> (float * float) * float

    /// Place each glyph's centre at its cumulative arc-distance along `path`, rotated to the tangent.
    let textOnPath (font: Text.FontData) (size: float) (paint: Paint) (path: PathFn) (text: string) : Cmd list =
        let mutable d = 0.0
        [ for i in 0 .. text.Length - 1 do
            let ch = string text.[i]
            let w = Text.measureWidth font size ch
            let (px, py), ang = path (d + w / 2.0)
            let m = Matrix3x2.multiply (Matrix3x2.rotate ang) (Matrix3x2.translate px py)
            // draw the glyph centred on the local origin (so it sits on the path point), then place
            yield Group(m, 1.0, [ DrawGlyphs(vec2 (-w / 2.0) (size * 0.34), Text.layout font size ch, paint) ])
            d <- d + w ]

    /// Text-on-circle path: glyphs ride a circle of radius `r` centred at (cx,cy), starting at
    /// `startRad`. `clockwise` flips direction; the baseline follows the arc.
    let circlePath (cx: float) (cy: float) (r: float) (startRad: float) (clockwise: bool) : PathFn =
        let dir = if clockwise then -1.0 else 1.0
        fun d ->
            let a = startRad + dir * d / r
            (cx + r * cos a, cy + r * sin a), a + dir * System.Math.PI / 2.0

    // ── motion along a curve ────────────────────────────────────────────────────────────────
    /// Cubic Bézier position at u∈[0,1].
    let cubicBezier (p0: float * float) (p1: float * float) (p2: float * float) (p3: float * float) (u: float) : float * float =
        let mt = 1.0 - u
        let b0, b1, b2, b3 = mt * mt * mt, 3.0 * mt * mt * u, 3.0 * mt * u * u, u * u * u
        let x = b0 * fst p0 + b1 * fst p1 + b2 * fst p2 + b3 * fst p3
        let y = b0 * snd p0 + b1 * snd p1 + b2 * snd p2 + b3 * snd p3
        (x, y)

    /// Cubic Bézier tangent angle (radians) at u — for orienting an object to its direction of travel.
    let cubicTangent (p0: float * float) (p1: float * float) (p2: float * float) (p3: float * float) (u: float) : float =
        let mt = 1.0 - u
        let dx = 3.0 * mt * mt * (fst p1 - fst p0) + 6.0 * mt * u * (fst p2 - fst p1) + 3.0 * u * u * (fst p3 - fst p2)
        let dy = 3.0 * mt * mt * (snd p1 - snd p0) + 6.0 * mt * u * (snd p2 - snd p1) + 3.0 * u * u * (snd p3 - snd p2)
        atan2 dy dx

    // ── seeded particles ────────────────────────────────────────────────────────────────────
    type Particle =
        { X0: float; Y0: float; VX: float; VY: float
          Size: float; Rot: float; Spin: float; Color: Color
          Birth: float<s>; Life: float<s>; Shape: int }      // Shape: 0 = rect (confetti), 1 = circle (spark)

    /// Deterministic hash → [0,1). Stable across runs/fps; vary `k` for independent streams per particle.
    let hash01 (seed: int) (i: int) (k: int) : float =
        let h = (i * 73856093) ^^^ (seed * 19349663) ^^^ (k * 83492791)
        let s = sin (float h) * 43758.5453
        s - floor s

    /// Simulate + draw particles under constant downward gravity (px/s²). Each fades out over its life.
    let drawParticles (gravity: float) (t: float<s>) (ps: Particle[]) : Cmd list =
        [ for p in ps do
            let lt = float (t - p.Birth)
            if lt >= 0.0 && lt < float p.Life then
                let x = p.X0 + p.VX * lt
                let y = p.Y0 + p.VY * lt + 0.5 * gravity * lt * lt
                let u = lt / float p.Life
                let op = clamp01 (1.0 - u * u)                       // ease-out fade
                let ang = p.Rot + p.Spin * lt
                let s = p.Size
                let m = aboutPivot x y (Matrix3x2.rotate ang)
                let shape =
                    if p.Shape = 1 then [ FillCircle(vec2 x y, (s / 2.0) * 1.0<px>, p.Color) ]
                    else [ FillRect(rect (x - s / 2.0) (y - s / 2.0) s (s * 0.6), p.Color) ]
                yield Group(m, op, shape) ]

    /// Confetti emitter: `n` pieces spawned across the top of the frame, drifting and tumbling down.
    let confetti (n: int) (seed: int) (palette: Color[]) (frame: Frame) : Particle[] =
        Array.init n (fun i ->
            let r = hash01 seed i
            { X0 = r 0 * frame.W
              Y0 = -40.0 - r 1 * 120.0
              VX = (r 2 - 0.5) * 160.0
              VY = 40.0 + r 3 * 120.0
              Size = 10.0 + r 4 * 14.0
              Rot = r 5 * 6.28
              Spin = (r 6 - 0.5) * 12.0
              Color = palette.[int (r 7 * float palette.Length) % (max 1 palette.Length)]
              Birth = (r 8 * 0.8) * 1.0<s>
              Life = (2.2 + r 9 * 1.5) * 1.0<s>
              Shape = 0 })

    /// Spark burst from (cx,cy): `n` sparks fly radially out and fade (use gravity 0 for a clean burst).
    let sparkBurst (n: int) (seed: int) (cx: float) (cy: float) (speed: float) (color: Color) : Particle[] =
        Array.init n (fun i ->
            let r = hash01 seed i
            let a = r 0 * 6.2831853
            let v = speed * (0.4 + r 1 * 0.6)
            { X0 = cx; Y0 = cy; VX = cos a * v; VY = sin a * v
              Size = 3.0 + r 2 * 4.0; Rot = 0.0; Spin = 0.0; Color = color
              Birth = 0.0<s>; Life = (0.5 + r 3 * 0.5) * 1.0<s>; Shape = 1 })

namespace VideoEdit

open System
open Rbt.Renderer

/// The library of effects (per-clip, time-varying) and transitions (A→B over an overlap),
/// all registered into the open registries by name. install() is idempotent and called by
/// the render loop. Add your own with registerEffect / registerTransition — no type edits.
module Fx =

    let private clamp01 x = max 0.0 (min 1.0 x)
    let private withAlpha (c: Color) (a: float) : Color =
        { c with A = byte (clamp01 a * float c.A |> round) }
    let private full (frame: Frame) = rect 0.0 0.0 frame.W frame.H
    let private centerOf (frame: Frame) = frame.W / 2.0, frame.H / 2.0
    let private id1 = Matrix3x2.identity
    let private easeOutCubic u = let p = 1.0 - u in 1.0 - p * p * p
    let private easeOutBack u =
        let c1 = 1.70158
        let c3 = c1 + 1.0
        let p = u - 1.0
        1.0 + c3 * p * p * p + c1 * p * p

    let mutable private installed = false

    let install () =
        if installed then () else
        installed <- true

        // ════════════ EFFECTS : frame -> clipDur -> params -> localT -> cmds -> cmds ════════════

        registerEffect "fadeIn" (fun _f _d ps t cmds ->
            let dn = pfloat ps "dur" 0.5
            [ Group(id1, (if dn <= 0.0 then 1.0 else clamp01 (float t / dn)), cmds) ])

        registerEffect "fadeOut" (fun _f dur ps t cmds ->
            let dn = pfloat ps "dur" 0.5
            let st = float dur - dn
            [ Group(id1, (if dn <= 0.0 then 1.0 else clamp01 (1.0 - (float t - st) / dn)), cmds) ])

        registerEffect "fade" (fun _f dur ps t cmds ->        // fade in AND out
            let di = pfloat ps "in" 0.4
            let dout = pfloat ps "out" 0.4
            let tt = float t
            let ai = if di <= 0.0 then 1.0 else clamp01 (tt / di)
            let ao = if dout <= 0.0 then 1.0 else clamp01 ((float dur - tt) / dout)
            [ Group(id1, min ai ao, cmds) ])

        registerEffect "opacity" (fun _f _d ps _t cmds ->
            [ Group(id1, clamp01 (pfloat ps "alpha" 1.0), cmds) ])

        registerEffect "zoom" (fun frame dur ps t cmds ->     // continuous linear zoom
            let z0 = pfloat ps "from" 1.0
            let z1 = pfloat ps "to" 1.12
            let u = if float dur <= 0.0 then 1.0 else clamp01 (float t / float dur)
            let z = z0 + (z1 - z0) * u
            let cx, cy = centerOf frame
            [ Group(aboutPivot cx cy (Matrix3x2.scale z z), 1.0, cmds) ])

        registerEffect "kenBurns" (fun frame dur ps t cmds ->
            let z0 = pfloat ps "from" 1.06
            let z1 = pfloat ps "to" 1.26
            let panX = pfloat ps "panX" 0.05
            let panY = pfloat ps "panY" 0.0
            let u = if float dur <= 0.0 then 1.0 else clamp01 (float t / float dur)
            let z = z0 + (z1 - z0) * u
            let cx, cy = centerOf frame
            let m = Matrix3x2.multiply (aboutPivot cx cy (Matrix3x2.scale z z))
                                       (Matrix3x2.translate (panX * frame.W * u) (panY * frame.H * u))
            [ Group(m, 1.0, cmds) ])

        registerEffect "pulse" (fun frame _d ps t cmds ->
            let amp = pfloat ps "amp" 0.05
            let freq = pfloat ps "freq" 3.0
            let z = 1.0 + amp * sin (float t * freq * 2.0 * Math.PI)
            let cx, cy = centerOf frame
            [ Group(aboutPivot cx cy (Matrix3x2.scale z z), 1.0, cmds) ])

        registerEffect "shake" (fun _f dur ps t cmds ->
            let amp = pfloat ps "amp" 12.0
            let decay = 1.0 - (if float dur <= 0.0 then 0.0 else clamp01 (float t / float dur))
            let dx = amp * decay * sin (float t * 53.0)
            let dy = amp * decay * sin (float t * 61.0 + 1.7)
            [ Group(Matrix3x2.translate dx dy, 1.0, cmds) ])

        registerEffect "spin" (fun frame dur ps t cmds ->
            let turns = pfloat ps "turns" 1.0
            let u = if float dur <= 0.0 then 1.0 else clamp01 (float t / float dur)
            let cx, cy = centerOf frame
            [ Group(aboutPivot cx cy (Matrix3x2.rotate (turns * 2.0 * Math.PI * u)), 1.0, cmds) ])

        registerEffect "wobble" (fun frame _d ps t cmds ->
            let amp = pfloat ps "amp" 0.06
            let freq = pfloat ps "freq" 2.0
            let cx, cy = centerOf frame
            [ Group(aboutPivot cx cy (Matrix3x2.rotate (amp * sin (float t * freq * 2.0 * Math.PI))), 1.0, cmds) ])

        registerEffect "floatY" (fun _f _d ps t cmds ->
            let amp = pfloat ps "amp" 14.0
            let freq = pfloat ps "freq" 0.8
            [ Group(Matrix3x2.translate 0.0 (amp * sin (float t * freq * 2.0 * Math.PI)), 1.0, cmds) ])

        registerEffect "riseIn" (fun _f _d ps t cmds ->
            let dn = pfloat ps "dur" 0.5
            let dist = pfloat ps "dist" 40.0
            let u = if dn <= 0.0 then 1.0 else clamp01 (float t / dn)
            let e = easeOutCubic u
            [ Group(Matrix3x2.translate 0.0 (dist * (1.0 - e)), e, cmds) ])

        registerEffect "scaleIn" (fun frame _d ps t cmds ->
            let dn = pfloat ps "dur" 0.45
            let from = pfloat ps "from" 0.6
            let u = if dn <= 0.0 then 1.0 else clamp01 (float t / dn)
            let z = from + (1.0 - from) * easeOutBack u
            let cx, cy = centerOf frame
            [ Group(aboutPivot cx cy (Matrix3x2.scale z z), clamp01 (u * 2.0), cmds) ])

        registerEffect "slideIn" (fun frame _d ps t cmds ->
            let dn = pfloat ps "dur" 0.5
            let dir = pstr ps "dir" "left"
            let u = if dn <= 0.0 then 1.0 else clamp01 (float t / dn)
            let off = 1.0 - easeOutCubic u
            let dx, dy =
                match dir with
                | "right" -> frame.W * off, 0.0
                | "up"    -> 0.0, -frame.H * off
                | "down"  -> 0.0, frame.H * off
                | _       -> -frame.W * off, 0.0
            [ Group(Matrix3x2.translate dx dy, 1.0, cmds) ])

        registerEffect "blur" (fun _f _d ps _t cmds ->
            let r = pint ps "radius" 8
            if r <= 0 then cmds else [ Filtered([ Blur r ], cmds) ])

        registerEffect "grayscale" (fun _f _d ps _t cmds -> [ Filtered([ Grayscale (pfloat ps "amount" 1.0) ], cmds) ])
        registerEffect "sepia"     (fun _f _d ps _t cmds -> [ Filtered([ Sepia (pfloat ps "amount" 1.0) ], cmds) ])
        registerEffect "saturate"  (fun _f _d ps _t cmds -> [ Filtered([ Saturate (pfloat ps "amount" 1.6) ], cmds) ])
        registerEffect "brightness"(fun _f _d ps _t cmds -> [ Filtered([ Brightness (pfloat ps "amount" 1.2) ], cmds) ])
        registerEffect "contrast"  (fun _f _d ps _t cmds -> [ Filtered([ Contrast (pfloat ps "amount" 1.3) ], cmds) ])

        registerEffect "hueRotate" (fun _f _d ps t cmds ->
            [ Filtered([ HueRotate (float t * pfloat ps "speed" 1.0 * 2.0 * Math.PI) ], cmds) ])

        registerEffect "tint" (fun frame _d ps _t cmds ->
            let c = pcolor ps "color" (Color.rgb 255uy 60uy 60uy)
            cmds @ [ FillRect(full frame, withAlpha c (pfloat ps "amount" 0.3)) ])

        registerEffect "vignette" (fun frame _d ps _t cmds ->
            let strength = pfloat ps "strength" 0.65
            let cx, cy = centerOf frame
            let radius = (sqrt (frame.W * frame.W + frame.H * frame.H)) / 2.0 * 1.0<px>
            let stops =
                [ { Offset = 0.0;  StopColor = Color.rgba 0uy 0uy 0uy 0uy }
                  { Offset = 0.55; StopColor = Color.rgba 0uy 0uy 0uy 0uy }
                  { Offset = 1.0;  StopColor = Color.rgba 0uy 0uy 0uy (byte (clamp01 strength * 255.0)) } ]
            cmds @ [ FillPath(rectPath (full frame), RadialGradient(vec2 cx cy, radius, stops, Pad), NonZero) ])

        registerEffect "flash" (fun frame _d ps t cmds ->
            let dn = pfloat ps "dur" 0.25
            let c = pcolor ps "color" Color.white
            let a = if dn <= 0.0 then 0.0 else clamp01 (1.0 - float t / dn)
            cmds @ (if a > 0.0 then [ FillRect(full frame, withAlpha c (a * a)) ] else []))

        registerEffect "glow" (fun _f _d ps _t cmds ->
            [ Filtered([ DropShadow(0.0, 0.0, pint ps "radius" 14, pcolor ps "color" (Color.rgb 255uy 220uy 80uy)) ], cmds) ])

        registerEffect "dropShadow" (fun _f _d ps _t cmds ->
            [ Filtered([ DropShadow(pfloat ps "dx" 0.0, pfloat ps "dy" 8.0, pint ps "radius" 12,
                                    pcolor ps "color" (Color.rgba 0uy 0uy 0uy 160uy)) ], cmds) ])

        // ════════════ TRANSITIONS : frame -> params -> p∈[0,1] -> out -> in -> cmds ════════════

        let crossfadeImpl _f _ps p (o: Cmd list) (i: Cmd list) =
            [ Group(id1, 1.0 - p, o); Group(id1, p, i) ]
        registerTransition "crossfade" crossfadeImpl
        registerTransition "dissolve" crossfadeImpl

        registerTransition "cut" (fun _f _ps p o i -> if p < 1.0 then o else i)

        registerTransition "slide" (fun frame ps p o i ->
            let dx, dy =
                match pstr ps "dir" "left" with
                | "right" -> -frame.W * (1.0 - p), 0.0
                | "up"    -> 0.0, frame.H * (1.0 - p)
                | "down"  -> 0.0, -frame.H * (1.0 - p)
                | _       -> frame.W * (1.0 - p), 0.0
            o @ [ Group(Matrix3x2.translate dx dy, 1.0, i) ])

        registerTransition "push" (fun frame ps p o i ->
            let (ox, oy), (ix, iy) =
                match pstr ps "dir" "left" with
                | "right" -> (frame.W * p, 0.0), (-frame.W * (1.0 - p), 0.0)
                | "up"    -> (0.0, -frame.H * p), (0.0, frame.H * (1.0 - p))
                | "down"  -> (0.0, frame.H * p), (0.0, -frame.H * (1.0 - p))
                | _       -> (-frame.W * p, 0.0), (frame.W * (1.0 - p), 0.0)
            [ Group(Matrix3x2.translate ox oy, 1.0, o); Group(Matrix3x2.translate ix iy, 1.0, i) ])

        registerTransition "wipe" (fun frame ps p o i ->
            let r =
                match pstr ps "dir" "left" with
                | "right" -> rect (frame.W * (1.0 - p)) 0.0 (frame.W * p) frame.H
                | "up"    -> rect 0.0 (frame.H * (1.0 - p)) frame.W (frame.H * p)
                | "down"  -> rect 0.0 0.0 frame.W (frame.H * p)
                | _       -> rect 0.0 0.0 (frame.W * p) frame.H
            o @ [ Cmd.Clip(rectPath r, NonZero, i) ])

        registerTransition "iris" (fun frame _ps p o i ->
            let cx, cy = centerOf frame
            let mr = (sqrt (frame.W * frame.W + frame.H * frame.H)) / 2.0
            o @ [ Cmd.Clip(Raster.ellipsePath cx cy (p * mr) (p * mr), NonZero, i) ])

        registerTransition "irisClose" (fun frame _ps p o i ->
            let cx, cy = centerOf frame
            let mr = (sqrt (frame.W * frame.W + frame.H * frame.H)) / 2.0
            i @ [ Cmd.Clip(Raster.ellipsePath cx cy ((1.0 - p) * mr) ((1.0 - p) * mr), NonZero, o) ])

        registerTransition "dipColor" (fun frame ps p o i ->
            let c = pcolor ps "color" Color.black
            let a = if p < 0.5 then 2.0 * p else 2.0 * (1.0 - p)
            let baseCmds = if p < 0.5 then o else i
            baseCmds @ [ FillRect(full frame, withAlpha c (clamp01 a)) ])

        registerTransition "zoomIn" (fun frame _ps p o i ->
            let cx, cy = centerOf frame
            let z = 0.5 + 0.5 * p
            [ Group(id1, 1.0 - p, o); Group(aboutPivot cx cy (Matrix3x2.scale z z), p, i) ])

        registerTransition "zoomOut" (fun frame _ps p o i ->
            let cx, cy = centerOf frame
            let z = 1.0 + 0.6 * p
            [ Group(aboutPivot cx cy (Matrix3x2.scale z z), 1.0 - p, o); Group(id1, p, i) ])

        registerTransition "spin" (fun frame _ps p o i ->
            let cx, cy = centerOf frame
            let ang = (1.0 - p) * Math.PI
            let z = 0.4 + 0.6 * p
            [ Group(id1, 1.0 - p, o)
              Group(aboutPivot cx cy (Matrix3x2.multiply (Matrix3x2.rotate ang) (Matrix3x2.scale z z)), p, i) ])

        registerTransition "whip" (fun frame ps p o i ->
            let blurR = int (round (40.0 * sin (Math.PI * p)))
            let (ox, oy), (ix, iy) =
                match pstr ps "dir" "left" with
                | "right" -> (frame.W * p, 0.0), (-frame.W * (1.0 - p), 0.0)
                | _       -> (-frame.W * p, 0.0), (frame.W * (1.0 - p), 0.0)
            let wrap (sx, sy) cmds =
                let g = [ Group(Matrix3x2.translate sx sy, 1.0, cmds) ]
                if blurR > 0 then [ Filtered([ Blur blurR ], g) ] else g
            (wrap (ox, oy) o) @ (wrap (ix, iy) i))

        registerTransition "scaleFade" (fun frame _ps p o i ->
            let cx, cy = centerOf frame
            [ Group(id1, 1.0 - p, o)
              Group(aboutPivot cx cy (Matrix3x2.scale (0.9 + 0.1 * p) (0.9 + 0.1 * p)), p, i) ])

    /// Names of everything registered (handy for the showcase + future LLM catalog).
    let catalog () =
        install ()
        effectNames (), transitionNames ()

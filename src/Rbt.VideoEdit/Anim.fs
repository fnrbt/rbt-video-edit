namespace Rbt.VideoEdit

open Rbt.Renderer

/// Pure, deterministic, time-driven animation. Easings are registry-resolved by name
/// (open + serializable); keyframe tracks are the canonical data form; the `A` module is
/// closure sugar over the same evaluator. No mutable per-frame state — frame i always
/// yields the same value, independent of fps.
[<AutoOpen>]
module Anim =

    /// An easing identity: a registry-resolved name + params. Open and serializable.
    type Easing = { Name: string; Params: Params }

    let private easings = Registry<Params -> float -> float>()

    module Ease =
        let register (name: string) (fn: Params -> float -> float) = easings.Register(name, fn)
        /// Evaluate normalized progress u in [0,1] through an easing (defaults to linear).
        let eval (e: Easing) (u: float) : float =
            match easings.TryGet e.Name with
            | Some fn -> fn e.Params u
            | None -> u
        let names () = easings.Names

        // typed convenience constructors so authoring stays clean
        let linear         = { Name = "linear"; Params = Map.empty }
        let easeInCubic    = { Name = "easeInCubic"; Params = Map.empty }
        let easeOutCubic   = { Name = "easeOutCubic"; Params = Map.empty }
        let easeInOutCubic = { Name = "easeInOutCubic"; Params = Map.empty }
        let easeOutBack (overshoot: float) = { Name = "easeOutBack"; Params = pmap [ "overshoot", PFloat overshoot ] }
        let spring (stiffness: float) (damping: float) =
            { Name = "spring"; Params = pmap [ "stiffness", PFloat stiffness; "damping", PFloat damping ] }
        let step = { Name = "step"; Params = Map.empty }
        let hold = { Name = "hold"; Params = Map.empty }

    // Built-in easing kernels are registered at module load — they depend only on math,
    // so they are always available (unlike effects/transitions, which need Builtins.install()).
    do
        Ease.register "linear" (fun _ u -> u)
        Ease.register "easeInCubic" (fun _ u -> u * u * u)
        Ease.register "easeOutCubic" (fun _ u -> let p = 1.0 - u in 1.0 - p * p * p)
        Ease.register "easeInOutCubic" (fun _ u ->
            if u < 0.5 then 4.0 * u * u * u
            else let p = -2.0 * u + 2.0 in 1.0 - (p * p * p) / 2.0)
        Ease.register "easeOutBack" (fun ps u ->
            let c1 = pfloat ps "overshoot" 1.70158
            let c3 = c1 + 1.0
            let p = u - 1.0
            1.0 + c3 * p * p * p + c1 * p * p)
        Ease.register "spring" (fun ps u ->
            let k = pfloat ps "stiffness" 8.0
            let d = pfloat ps "damping" 4.0
            if u >= 1.0 then 1.0
            else 1.0 - exp (-d * u) * cos (sqrt k * u))
        Ease.register "step" (fun _ u -> if u >= 1.0 then 1.0 else 0.0)
        Ease.register "hold" (fun _ _ -> 0.0)

    let clampByte (v: float) : byte = byte (max 0.0 (min 255.0 (round v)))

    // ── Keyframe tracks (serializable, canonical) ──
    type Keyframe = { Time: float; Value: float; Ease: Easing }     // Ease = segment leading INTO this key
    type ScalarTrack = { Keys: Keyframe[] }                          // assumed sorted by Time; clamps at ends
    type ColorTrack = { R: ScalarTrack; G: ScalarTrack; B: ScalarTrack; A: ScalarTrack }
    type Vec2Track = { X: ScalarTrack; Y: ScalarTrack }

    let evalScalar (track: ScalarTrack) (t: float) : float =
        let ks = track.Keys
        if ks.Length = 0 then 0.0
        elif t <= ks.[0].Time then ks.[0].Value
        elif t >= ks.[ks.Length - 1].Time then ks.[ks.Length - 1].Value
        else
            let mutable i = 1
            while i < ks.Length && ks.[i].Time <= t do i <- i + 1
            let a = ks.[i - 1]
            let b = ks.[i]
            let span = b.Time - a.Time
            let u = if span <= 0.0 then 1.0 else (t - a.Time) / span
            a.Value + (b.Value - a.Value) * Ease.eval b.Ease u

    let evalColor (track: ColorTrack) (t: float) : Color =
        { R = clampByte (evalScalar track.R t)
          G = clampByte (evalScalar track.G t)
          B = clampByte (evalScalar track.B t)
          A = clampByte (evalScalar track.A t) }

    let evalVec2 (track: Vec2Track) (t: float) : Vec2 =
        vec2 (evalScalar track.X t) (evalScalar track.Y t)

    // ── Closure DSL (sugar over the evaluator) ──
    type Anim<'T> = float -> 'T

    module A =
        let constant (v: 'T) : Anim<'T> = fun _ -> v
        let tween (from: float) (to': float) (dur: float) (e: Easing) : Anim<float> =
            fun t ->
                let u =
                    if dur <= 0.0 then 1.0
                    elif t <= 0.0 then 0.0
                    elif t >= dur then 1.0
                    else t / dur
                from + (to' - from) * Ease.eval e u
        let delay (d: float) (a: Anim<'T>) : Anim<'T> = fun t -> a (t - d)
        let map (f: 'a -> 'b) (a: Anim<'a>) : Anim<'b> = fun t -> f (a t)
        let map2 (f: 'a -> 'b -> 'c) (a: Anim<'a>) (b: Anim<'b>) : Anim<'c> = fun t -> f (a t) (b t)
        let lerp (a: float) (b: float) (u: float) = a + (b - a) * u
        let lerpColor (c0: Color) (c1: Color) (u: float) : Color =
            let l (x: byte) (y: byte) = clampByte (lerp (float x) (float y) u)
            { R = l c0.R c1.R; G = l c0.G c1.G; B = l c0.B c1.B; A = l c0.A c1.A }

namespace Rbt.VideoEdit

open Rbt.Renderer

/// The full-render loop: sample the clip at each frame, rasterize headlessly, encode to MP4.
/// Frames render in parallel (one rasterizer backend per worker slot) and are written to the
/// encoder in order. This is safe because the renderer's per-frame scratch pool is thread-local
/// and its text/glyph/font caches are locked — so concurrent frames never race. Set VE_SERIAL=1
/// to force the single-threaded path (determinism debugging / parity checks).
[<AutoOpen>]
module Render =

    /// Render a single frame `i` to a framebuffer using the given backend.
    let renderFrameTo (cfg: RenderConfig) (frame: Frame) (root: Clip) (backend: Backend) (i: int) : Framebuffer =
        let t = (float i / float cfg.Fps) * 1.0<s>
        Rbt.Renderer.Scene.render backend cfg.W cfg.H (Clear cfg.Background :: eval frame root t)

    /// A cache of rendered frames keyed by frame index, for INCREMENTAL re-render: after an edit,
    /// invalidate only the changed time-range and re-render reuses every untouched frame. This is
    /// what turns a full re-render into "near-instant feedback on the changed section" in the edit
    /// loop. (Holds whole frames, so it's for bounded-length pieces / the interactive preview.)
    type FrameCache() =
        let frames = System.Collections.Generic.Dictionary<int, Framebuffer>()
        member _.TryGet(i) = match frames.TryGetValue i with | true, v -> Some v | _ -> None
        member _.Put(i, fb) = frames.[i] <- fb
        member _.InvalidateAll() = frames.Clear()
        member _.Count = frames.Count
        /// Drop cached frames overlapping [t0, t1] so the next render recomputes just that span.
        member _.InvalidateRange(t0: float<s>, t1: float<s>, fps: int) =
            let a = int (floor (float t0 * float fps))
            let b = int (ceil (float t1 * float fps))
            for i in a .. b do frames.Remove i |> ignore

    /// Like `renderToMp4` but reuses frames already in `cache`, rendering only the missing ones
    /// (in parallel) and storing them back. Returns the number of frames freshly rendered.
    let renderToMp4Cached (cfg: RenderConfig) (root: Clip) (cache: FrameCache) : int =
        Fx.install ()
        Templates.install ()
        let frame = { W = float cfg.W; H = float cfg.H }
        let total = int (ceil (float (duration root) * float cfg.Fps))
        let audio = collectAudio root
        let enc = new FfmpegCliEncoder(cfg, audio, duration root) :> Encoder
        let par = max 1 (System.Environment.ProcessorCount - 1)
        let backends = Array.init par (fun _ -> Raster.create ())
        let mutable rendered = 0
        try
            let mutable i = 0
            while i < total do
                let n = min par (total - i)
                let results : Framebuffer[] = Array.zeroCreate n
                // render missing frames in parallel (cache reads are concurrent-safe: no writes here)
                System.Threading.Tasks.Parallel.For(0, n, fun k ->
                    match cache.TryGet(i + k) with
                    | Some fb -> results.[k] <- fb
                    | None -> results.[k] <- renderFrameTo cfg frame root backends.[k] (i + k)) |> ignore
                // store newly-rendered frames (serial → no dict races), then encode in order
                for k in 0 .. n - 1 do
                    if (cache.TryGet(i + k)).IsNone then cache.Put(i + k, results.[k]); rendered <- rendered + 1
                for k in 0 .. n - 1 do enc.WriteFrame results.[k]
                i <- i + n
        finally
            enc.Finish()
        rendered

    let renderToMp4 (cfg: RenderConfig) (root: Clip) : unit =
        Fx.install ()
        Templates.install ()
        let frame = { W = float cfg.W; H = float cfg.H }
        let total = int (ceil (float (duration root) * float cfg.Fps))
        let audio = collectAudio root
        let enc = new FfmpegCliEncoder(cfg, audio, duration root) :> Encoder
        let par =
            if System.Environment.GetEnvironmentVariable "VE_SERIAL" = "1" then 1
            else max 1 (System.Environment.ProcessorCount - 1)
        let sw = System.Diagnostics.Stopwatch.StartNew()
        try
            if par <= 1 then
                let backend = Raster.create ()
                for i in 0 .. total - 1 do enc.WriteFrame(renderFrameTo cfg frame root backend i)
            else
                // One backend per worker slot; render frames in parallel batches of `par`, write
                // each batch to the encoder IN ORDER. Bounded memory (~`par` frames in flight).
                let backends = Array.init par (fun _ -> Raster.create ())
                let mutable i = 0
                while i < total do
                    let n = min par (total - i)
                    let results : Framebuffer[] = Array.zeroCreate n
                    System.Threading.Tasks.Parallel.For(0, n, fun k ->
                        results.[k] <- renderFrameTo cfg frame root backends.[k] (i + k)) |> ignore
                    for k in 0 .. n - 1 do enc.WriteFrame results.[k]
                    i <- i + n
        finally
            enc.Finish()
        printfn "Wrote %s — %d frames @ %dfps (%.2fs), %d audio track(s), %d thread(s) in %.1fs"
            cfg.Output total cfg.Fps (float (duration root)) (List.length audio) par sw.Elapsed.TotalSeconds

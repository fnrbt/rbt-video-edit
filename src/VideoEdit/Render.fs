namespace VideoEdit

open Renderer

/// The full-render loop: sample the clip at each frame, rasterize headlessly, encode to MP4.
[<AutoOpen>]
module Render =

    let renderToMp4 (cfg: RenderConfig) (root: Clip) : unit =
        Fx.install ()
        Templates.install ()
        let frame = { W = float cfg.W; H = float cfg.H }
        let backend = Raster.create ()
        let total = int (ceil (float (duration root) * float cfg.Fps))
        let audio = collectAudio root
        let enc = new FfmpegCliEncoder(cfg, audio, duration root) :> Encoder
        let sw = System.Diagnostics.Stopwatch.StartNew()
        try
            // Overlays/components carry their own antialias pass; footage and flat backgrounds
            // are drawn directly, so the loop just clears and evaluates.
            for i in 0 .. total - 1 do
                let t = (float i / float cfg.Fps) * 1.0<s>
                let cmds = Clear cfg.Background :: eval frame root t
                enc.WriteFrame(Scene.render backend cfg.W cfg.H cmds)
        finally
            enc.Finish()
        printfn "Wrote %s — %d frames @ %dfps (%.2fs), %d audio track(s) in %.1fs"
            cfg.Output total cfg.Fps (float (duration root)) (List.length audio) sw.Elapsed.TotalSeconds

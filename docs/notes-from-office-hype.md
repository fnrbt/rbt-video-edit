# Notes from the office-hype video (2026-11-14)

Found while editing a ~35 s product video entirely in F# script
(`~/Dev/office-hype/edit/hype.fsx`): browser recordings framed in a floating
window, a camera moving inside high-resolution footage, a drawn cursor with
click rings, captions, kinetic titles, a montage and crossfades. The library
held up: nothing crashed or rendered wrong, and the 35 s 1080p cut renders in
about a minute on 9 threads.

## Fixed (this commit)

1. **Unit friction**: `Draw` (Draw.fs) gives plain-float constructors for the
   commands whose parameters are `float<px>` (stroke width and dash offset,
   circle radius, rounded-rect radius, shadow spread and corner radius,
   radial gradient radius), plus `roundedRectPath`, gradients, `textWidth`
   and `text`. The root cause is in Rbt.Renderer's `Cmd`, which mixes `float`
   and `float<px>` (a `BoxShadow`'s offsets are `float`, its spread `float<px>`).
   Making that consistent is a breaking change for every renderer consumer;
   left for a renderer release.
2. **Fonts from the working directory**: `Theme.fontDir` finds the bundled
   fonts from RBT_VIDEO_FONTS, next to the assembly (the build now copies
   `fonts/*.ttf` there and packs them as content), or by walking up from the
   assembly. `Theme.load` resolves a relative name there when it does not exist
   from the working directory.
3. **No still output**: `Png.encode` / `Png.save` (a small PNG writer, no
   ffmpeg) and `Render.saveFramePng cfg clip t path`.

## Open (come back to these)

4. **Using it from a script**: `#r "nuget: Rbt.VideoEdit"` needs the Forge
   feed configured; without it the only way is to build a project (the
   Sample) and `#I` its bin folder for the Rbt.* dependency DLLs. A
   `scripts/` folder with a ready `#load`-able reference file, or a documented
   NuGet.Config for fsi, would fix it.
5. **Footage memory and inputs**: `Video.stream` always decodes at the size it
   is asked for, into a 64-frame window. Footage that is cropped and zoomed into
   needs native size (3200x1800 for 2x browser captures), which is ~1.4 GB per
   stream. There is no still-image (PNG/JPEG) source and no image-sequence
   source: captures had to be encoded to MP4 first, and a single still is read
   with `Video.loadClip` (an ffmpeg run). Wanted: a window size parameter,
   decoding a region, `Image.load`, and an image-sequence source.
6. **No screencast primitives**: hype.fsx hand-writes what any product video
   needs, and it would make a good module. It covers a cursor drawn from a
   recorded path, click rings, captions from an action log, a camera (centre +
   zoom, moved by one ease per shot) over high-resolution footage, a
   floating-window frame with shadow and light sweep, and a fanned montage of
   stills. The capture side (a frame-stepped Playwright recorder that writes
   raw.mp4 plus events.json) is `~/Dev/office-hype/capture/capture.mjs`.
7. **The s-expression format can't express it**: the cursor, rings and camera
   are `Graphic` escape hatches, so the video was written against the F# API.
   For agents that only write sexp documents, the screencast primitives
   (item 6) would need sexp forms (e.g. `(screencast "raw.mp4" :events
   "events.json" :camera (glide ...))`).

Smaller things noticed:
- `kineticText` lays glyphs out one by one, without kerning (fine for display text).
- `Place3D` needs a full `Xf3` record when built outside Dsl's helpers (`withXf3` is private).

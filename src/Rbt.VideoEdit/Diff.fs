namespace Rbt.VideoEdit

// NOTE: deliberately does NOT `open Renderer` — its `Cmd` DU has `Mask`/`Clip`/… cases that
// would shadow our `Timeline` clip cases in these pattern matches. Diff only needs Core+Timeline.

/// A structural diff over the clip AST. The data-carrying parts (template/effect/transition
/// names + params, durations, structure, audio) diff into human-readable changes; closure-
/// carrying leaves (raw graphics, mask shapes, animated transforms) are opaque and only their
/// presence / duration is compared. This is what makes "LLM edits the program, we diff the
/// resulting AST" read like the edits a person would describe.
[<AutoOpen>]
module Diff =

    let private caseName (c: Clip) : string =
        match c with
        | Blank _ -> "blank" | SolidBg _ -> "solidBg" | GradientBg _ -> "gradientBg"
        | Graphic _ -> "graphic" | Audio _ -> "audio" | Concat _ -> "concat" | Over _ -> "over"
        | Trim _ -> "trim" | Shift _ -> "shift" | Transform _ -> "transform" | Place _ -> "place"
        | Mask _ -> "mask" | Reframe _ -> "reframe" | Speed _ -> "speed" | Apply (e, _) -> "fx:" + e.Name
        | Between (t, _, _) -> "transition:" + t.Name | Component (_, r) -> "component:" + r.Name | Inst r -> "inst:" + r.Name
        | Section (n, _, _) -> "section:" + n
        | TimeMap _ -> "timemap" | Blended (m, _) -> "blend:" + sprintf "%A" m | Place3D _ -> "place3d"

    let diff (before: Clip) (after: Clip) : string list =
        let out = ResizeArray<string>()
        let pdiff (path: string) (pa: Params) (pb: Params) =
            let keys = Set.ofSeq (Seq.append (Map.toSeq pa |> Seq.map fst) (Map.toSeq pb |> Seq.map fst))
            for k in Set.toList keys do
                match Map.tryFind k pa, Map.tryFind k pb with
                | Some va, Some vb -> if showParam va <> showParam vb then out.Add(sprintf "~ %s.%s: %s → %s" path k (showParam va) (showParam vb))
                | None, Some vb -> out.Add(sprintf "+ %s.%s = %s" path k (showParam vb))
                | Some va, None -> out.Add(sprintf "- %s.%s (was %s)" path k (showParam va))
                | None, None -> ()
        let durChg path tag (da: float<s>) (db: float<s>) =
            if da <> db then out.Add(sprintf "~ %s %s: %.2fs → %.2fs" path tag (float da) (float db))
        let rec go (path: string) (a: Clip) (b: Clip) =
            match a, b with
            | Component (da, ra), Component (db, rb) ->
                if ra.Name <> rb.Name then out.Add(sprintf "~ %s: component %s → %s" path ra.Name rb.Name)
                else durChg path ra.Name da db; pdiff (sprintf "%s.%s" path ra.Name) ra.Params rb.Params
            | Inst ra, Inst rb ->
                if ra.Name <> rb.Name then out.Add(sprintf "~ %s: template %s → %s" path ra.Name rb.Name)
                else pdiff (sprintf "%s.%s" path ra.Name) ra.Params rb.Params
            | Apply (ea, ca), Apply (eb, cb) ->
                if ea.Name <> eb.Name then out.Add(sprintf "~ %s: effect %s → %s" path ea.Name eb.Name)
                else pdiff (sprintf "%s.fx:%s" path ea.Name) ea.Params eb.Params
                go path ca cb
            | Between (ta, a1, a2), Between (tb, b1, b2) ->
                if ta.Name <> tb.Name then out.Add(sprintf "~ %s: transition %s → %s" path ta.Name tb.Name)
                else durChg path ta.Name ta.Dur tb.Dur; pdiff (sprintf "%s.tr:%s" path ta.Name) ta.Params tb.Params
                go (path + ".a") a1 b1; go (path + ".b") a2 b2
            | Concat xs, Concat ys ->
                if List.length xs <> List.length ys then out.Add(sprintf "~ %s: %d → %d clips" path (List.length xs) (List.length ys))
                let n = min (List.length xs) (List.length ys)
                List.iteri2 (fun i x y -> go (sprintf "%s[%d]" path i) x y) (List.truncate n xs) (List.truncate n ys)
                ys |> List.skip n |> List.iteri (fun i y -> out.Add(sprintf "+ %s[%d]: %s" path (n + i) (caseName y)))
                xs |> List.skip n |> List.iteri (fun i x -> out.Add(sprintf "- %s[%d]: %s" path (n + i) (caseName x)))
            | Over (a1, a2), Over (b1, b2) -> go (path + ".under") a1 b1; go (path + ".over") a2 b2
            | Section (na, ea, ca), Section (nb, eb, cb) ->
                if na <> nb then out.Add(sprintf "~ %s: group %s → %s" path na nb)
                if ea <> eb then out.Add(sprintf "~ %s '%s': %s" path nb (if eb then "unmuted" else "muted"))
                go (sprintf "%s.%s" path nb) ca cb
            | Trim (ca, sa, la), Trim (cb, sb, lb) ->
                if sa <> sb || la <> lb then out.Add(sprintf "~ %s trim: [%.2f,%.2f] → [%.2f,%.2f]" path (float sa) (float la) (float sb) (float lb))
                go path ca cb
            | Shift (ca, da), Shift (cb, db) -> durChg path "shift" da db; go path ca cb
            | Speed (ca, fa), Speed (cb, fb) -> (if fa <> fb then out.Add(sprintf "~ %s speed: %.3g → %.3g" path fa fb)); go path ca cb
            | Transform (ca, _), Transform (cb, _) | Place (ca, _), Place (cb, _) | Place3D (ca, _), Place3D (cb, _)
            | Mask (ca, _), Mask (cb, _) | Reframe (ca, _), Reframe (cb, _)
            | Blended (_, ca), Blended (_, cb) | TimeMap (ca, _), TimeMap (cb, _) -> go path ca cb
            | SolidBg (da, ca), SolidBg (db, cb) ->
                durChg path "solidBg" da db
                if showParam (PColor ca) <> showParam (PColor cb) then out.Add(sprintf "~ %s solidBg.color: %s → %s" path (showParam (PColor ca)) (showParam (PColor cb)))
            | Audio (da, ra), Audio (db, rb) ->
                durChg path "audio" da db
                if ra <> rb then out.Add(sprintf "~ %s audio: %s → %s" path ra.Path rb.Path)
            | Graphic (da, _), Graphic (db, _) -> durChg path "graphic(opaque)" da db
            | Blank da, Blank db -> durChg path "blank" da db
            | GradientBg (da, _), GradientBg (db, _) -> durChg path "gradientBg" da db
            | _ -> if caseName a <> caseName b then out.Add(sprintf "~ %s: %s → %s" path (caseName a) (caseName b))
        go "root" before after
        List.ofSeq out

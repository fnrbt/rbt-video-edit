namespace VideoEdit

open Renderer

/// Registers the built-in components as TEMPLATES (name + flat params), so a graphic can be a
/// data reference (`Component(dur, {Name;Params})`) instead of an opaque closure. The closure
/// impl lives here, behind the name — the AST only carries the reference, which is diffable.
/// Add your own with `registerComponent` / `registerClipTemplate` — no type edits.
module Templates =

    let mutable private installed = false

    let install () =
        if installed then () else
        installed <- true

        registerComponent "lowerThird" (fun ps ->
            Components.lowerThird
                { Name = pstr ps "name" ""; Title = pstr ps "title" ""
                  Accent = pcolor ps "accent" Theme.gold })

        registerComponent "centeredLabel" (fun ps ->
            Components.centeredLabel (pstr ps "text" "") (pfloat ps "size" 92.0) (pcolor ps "color" Theme.white))

        registerComponent "titleCard" (fun ps ->
            Components.titleCard
                { Headline = pstr ps "headline" ""
                  Subtitle = (match pstr ps "subtitle" "" with "" -> None | s -> Some s)
                  Size = pfloat ps "size" 96.0
                  Fill = pcolor ps "fill" Theme.gold
                  Shadow = pcolor ps "shadow" Theme.shadowStrong })

        registerComponent "caption" (fun ps -> Components.caption (pstr ps "text" ""))

        registerComponent "cornerBadge" (fun ps ->
            Components.cornerBadge (pstr ps "text" "") (pcolor ps "fill" Theme.gold))

        registerComponent "banner" (fun ps ->
            Components.banner { Text = pstr ps "text" ""; Fill = pcolor ps "fill" Theme.gold; Accent = pcolor ps "accent" Theme.white })

        registerComponent "bulletList" (fun ps ->
            Components.bulletList (pstrs ps "items" []) (pcolor ps "accent" Theme.gold))

    /// Names of all registered component + clip templates (for an LLM/inspector catalog).
    let catalog () =
        install ()
        componentNames (), clipTemplateNames ()

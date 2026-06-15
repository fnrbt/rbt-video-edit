; A footage piece — assets, a parameterized chapter template, effect pipes, a PiP, and music.
(video (1280 720) :fps 30 :bg #0a0e1c :out "out/sx_film.mp4"

  (assets
    (jelly "assets/jellyfish.mp4" :len 4s)
    (city  "assets/city.mp4" :from 2s :len 4s)
    (bunny "assets/bunny.mp4" :from 2s :len 3s)
    (bed   "assets/bed.m4a"))

  ; a chapter = footage (ken-burns + saturate) under a lower-third
  (template chapter (src name title (accent #FFD200))
    (layer
      (-> (video $src :len 4s) (kenBurns) (saturate :amount 1.12))
      (component lowerThird :name $name :title $title :accent $accent)))

  (timeline
    (layer
      ; the main sequence: two chapters, crossfaded
      (seq :gap (crossfade 0.7s)
        (chapter :src jelly :name "The Deep"  :title "Chapter One")
        (chapter :src city  :name "The Grid"  :title "Chapter Two" :accent #00EAFF))
      ; a circular facecam PiP of a third clip, placed + slid in, over the whole thing
      (place :pip (360 150 0.34) :enterFrom (0 360 0.7s)
        (mask (circle) (-> (video bunny :len 3s) (saturate :amount 1.1))))
      ; one music bed across the whole timeline (inherits its length)
      (music bed :gain 0.4 :fadeIn 1s :fadeOut 1s))))

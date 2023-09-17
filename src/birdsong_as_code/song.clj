(ns birdsong-as-code.song
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [overtone.inst.drum :as drums]))

; Generic machinery
(defsynth walker [out-bus 0 freq 0.5]
  (out:kr out-bus (lf-noise1:kr freq)))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk))
(def resonance (mul-add (in:kr random-walk) 1000 5000))

(defcgen cut-out [input {:default :none}]
  (:ar (let [_ (detect-silence input :action FREE)]
         input))
  (:default :ar))

(defcgen effects [input  {:default :none}
                  pan    {:default 0}
                  wet    {:default 0.33}
                  room   {:default 0.5}
                  volume {:default 1.0}
                  early  {:default 0.1}
                  high   {:default 20000}
                  low    {:default 0}]
  (:ar (-> input
           (* volume)
           (pan2 pan)
           (free-verb :mix wet :room room)
           (lpf high)
           cut-out))
  (:default :ar))


(definst corgan [freq 440 dur 10.0 depth 1 walk 1 attack 0.01 under-attack 0.3 vol 1.0 pan 0.0 wet 0.5 room 0.5 vibrato 3 limit 99999 gate 1.0]
  (->
    (saw freq)
    (* (env-gen (perc 0.01 dur)))
    (rlpf (mul-add (sin-osc vibrato) (line:kr 0 200 dur) (* freq 4)) 0.3)
    (* vol 4)
    (clip2 0.2)
    (+ (* 1/3 (sin-osc freq) (env-gen (perc under-attack dur))))
    (+ (* (lpf (* 1 (brown-noise)) 500) (env-gen (perc 0.01 0.2))))
    (* (env-gen (adsr attack 0.05 1) (* gate (line:kr 1.0 0.0 dur))))
    (+ (* 2 (sin-osc freq) (env-gen (perc 0.01 0.1))))
    (rlpf (* walk resonance) 0.1)
    (* vol 3)
    (effects :pan pan :wet wet :room room :volume vol :high limit)))

(definst blorp [freq 440 dur 1.0 depth 1 walk 1 attack 0.01 under-attack 0.3 vol 1.0 pan 0.0 wet 0.5 room 0.5 vibrato 3 limit 99999 gate 1.0]
  (->
    (square freq)
    (+ (* 3 (sin-osc freq) (env-gen (perc under-attack dur))))
    (* (env-gen (perc 0.001 0.4)))
    (lpf (line:kr (* freq 9) (* freq 1) dur))
    (* vol 9)
    (clip2 0.4)
    (* 3)
    (rlpf (* walk resonance) 0.1)
    (* vol 3/2)
    (effects :pan pan :wet wet :room room :volume vol :high limit)))

(definst whistle [freq 1320 dur 1.0 volume 1.0 pan 0.0 wet 0.5 room 0.5 limit 12000]
  (let [freq (* freq (+ 1 (* -0.02 (env-gen (perc 0.15 0.15)))))]
    (-> (sin-osc freq)
        (+ (* 1/5 (sin-osc (* 2 freq))))
        (+ (* 1/8 (sin-osc (* 3 freq))))
        (+ (* 1/14 (sin-osc (* 4 freq))))
        (+ (* 1/25 (sin-osc (* 5 freq))))
        (lpf (+ freq (* freq 5 (env-gen (perc 0.1 (- dur 0.1))))))
        (* (env-gen (perc (min 0.3 dur) (- dur 0.3))))
        (+ (-> (white-noise) (* 1/60) (rhpf freq 0.1) (* (env-gen (perc 0.15 0.15)))))
        (* volume)
        (effects :pan pan :wet wet :room room :volume volume :high limit))))

(defmethod live/play-note :default [{hertz :pitch seconds :duration volume :velocity}]
  (when hertz (whistle hertz seconds (or volume 1.0))))

(definst butcherbird-15 []
  (let [buffer (load-sample "recordings/AUDIO 15.wav")]
    (play-buf 1 buffer :action FREE :rate 1.0)))

(definst butcherbird-19 []
  (let [buffer (load-sample "recordings/AUDIO 19.wav")]
    (play-buf 1 buffer :action FREE :rate 1.0)))

(definst butcherbird-23 []
  (let [buffer (load-sample "recordings/AUDIO 23.wav")]
    (play-buf 1 buffer :action FREE :rate 1.0)))

(definst butcherbird-24 []
  (let [buffer (load-sample "recordings/AUDIO 24.wav")]
    (play-buf 1 buffer :action FREE :rate 1.0)))

(definst butcherbird-23-transposed []
  (let [buffer (load-sample "recordings/AUDIO 23.wav")]
    (play-buf 1 buffer :action FREE :rate 132/110)))

(def butcherbirds
  {15   butcherbird-15
   19   butcherbird-19
   23   butcherbird-23
   23.3 butcherbird-23-transposed
   24   butcherbird-24})

(def phrase-24
   [{:time 0 :duration 8 :bird 24 :part :butcherbird}])

(def transcription-24-raw
  (let [a (phrase
            [0.256 0.187 0.185 0.956 0.595]
            [ 1454  1303  1303  1559  1567])
        b (phrase
            [0.175 0.272 0.520]
            [ 1111  1111  1043])
        a' (phrase
             [0.298 0.424 0.436]
             [ 2348  2348  2093])
        b' (phrase
             [0.063 0.238 0.393 0.794]
             [ 1432  1300  1300  1583])]
    (->> (after 0.462 a) (then b) (then a') (then b'))))

(comment
  (->> transcription-24-raw
       quiet
       (with phrase-24)
       live/play)
)

(definst hermit-thrush-02 []
  (let [buffer (load-sample "recordings/pnas.1406023111.sa02.wav")]
    (play-buf 1 buffer :action FREE :rate 1.0)))

(definst hermit-thrush-04 []
  (let [buffer (load-sample "recordings/pnas.1406023111.sa04.wav")]
    (play-buf 1 buffer :action FREE :rate 1.0)))

(def hermit-thrushes
  {02 hermit-thrush-02
   04 hermit-thrush-04})

(def quiet (partial all :velocity 0.3))

(defn mean [xs]
  (/ (reduce + xs)  (count xs)))

(defn pow2 [x] (* x x))

(def pitches (partial map :pitch))

(defn mean-difference-squared [xs ys]
    (->> (map - xs ys)
         (map pow2)
         mean))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIRDSONG AS CODE                           ;;;
;;;                                            ;;;
;;; Chris Ford                                 ;;;
;;; https://github.com/ctford/birdsong-as-code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (* 3/2 220)
  (+ 440 110)
  (butcherbird-23)
)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Representation matters ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def raw [1 2 2 3 3 3 4 4 4 4 5 5 5 5 5 6 6 6 6 6 6 7 7 7 7 7 7 7])

(def computed
  (->>
    (range 1 8)
    (map (fn [n] (repeat n n)))
    (reduce concat)))

(comment
  (= raw computed)
)





;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Zoömusicology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  "The study of music in animal culture"
)

(def source1
  {:title    "Is Birdsong Music?"
   :type     :book,
   :authors ["Hollis Taylor"]
   :bird     "Pied Butcherbird"})

(def source2
  {:title    "Overtone-based pitch selection in hermit thrush song",
   :type     :article,
   :authors ["Doolittle", "Gingras", "Endres", "Fitch"]
   :bird     "Hermit Thrush"})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Birdsong is music  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (butcherbird-19)
  (hermit-thrush-02)
)












;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frequency is pitch ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst tone [frequency 440]
  (sin-osc frequency))

(comment
  (tone 300)
)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shaping notes         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst beep [frequency 440 volume 1.0]
  (let [envelope (env-gen (perc 0.3 0.9) :action FREE)]
    (-> (sin-osc frequency)
        (* envelope volume))))

(comment
    (beep 500)
)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fractions sound good  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (do
    (beep 300)
    (beep 500))
)











;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Harmonics          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst boop [frequency 440 volume 1.0]
  (let [envelope (env-gen (perc 0.3 0.9) :action FREE)]
    (-> (+ (* 1/1 (sin-osc (* 1 frequency))))
        (+ (* 1/2 (sin-osc (* 2 frequency))))
        (+ (* 1/3 (sin-osc (* 3 frequency))))
        (+ (* 1/4 (sin-osc (* 4 frequency))))
        (+ (* 1/5 (sin-osc (* 5 frequency))))
        (* envelope 2/5 volume))))

(comment
  (beep 500)
  (do
    (boop 300)
    (boop 500))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logarithmic scale  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn logarithmic [root]
  (let [twelfth-root (Math/pow 2 1/12)]
    (fn [n] (* root (Math/pow twelfth-root n)))))

(def logarithmic-scale
  (->> (phrase
         (repeat 1/2)                     ; Durations (all half a second)
         [12 14 16 17 19 21 22 23 24])       ; Pitches
       (where :pitch (logarithmic 440)))) ; Put pitches in the logarithmic scale

(comment
  (->> logarithmic-scale live/play)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linear scale       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn linear [root]
  (fn [n] (* root n)))

(def linear-scale
  (->> (phrase
         (repeat 1/2)                ; Durations (all half a second)
         [8 9 10 11 12 13 14 15 16]) ; Pitches
       (where :pitch (linear 110)))) ; Put pitches in the linear scale

(comment
  (->> linear-scale live/play)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compare and contrast ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (->> logarithmic-scale
       (with linear-scale)
       live/play)
)

(comment
  (let [scale (logarithmic 100)]
    (/ (scale 4) (scale 0)))    ; Approximately 5/4

  (let [scale (linear 100)]
    (/ (scale 10) (scale 8)))   ; Exactly 5/4
)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Species call       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def species-call
  (->> (phrase
         [1/8 1/8 1/4]    ; Durations
         [ 18  18  16]))) ; Pitches

(comment
  (->> species-call (where :pitch (linear 110)) live/play)
  (butcherbird-15)
)







;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Audio 24           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def transcription-24-linear
  (let [a (->> (phrase
                 [0.256 0.187 0.185 0.956 0.595]
                 [   11    10    10    12    12]))
        b (->> (phrase
                 [0.175 0.272 0.520]
                 [    9     9     8]))
        a' (->> (phrase
                  [0.298 0.424 0.436]
                  (pitches species-call)))
        b' (->> (phrase
                  [0.063 0.238 0.393 0.794]
                  [   11    10    10    12]))]
    (->> (after 0.462 a) (then b) (then a') (then b')
         (where :pitch (linear 130)))))

(comment
  (->> (quiet transcription-24-linear)
       (with phrase-24)
       live/play)
)

(def transcription-24-logarithmic
  (let [a (phrase
            [0.256 0.187 0.185 0.956 0.595]
            [   53    52    52    55    55])
        b (phrase
            [0.175 0.272 0.520]
            [   50    50    48])
        a' (phrase
             [0.298 0.424 0.436]
             [   62    62    60])
        b' (phrase
             [0.063 0.238 0.393 0.794]
             [   53    52    52    55])]
    (->> (after 0.462 a) (then b) (then a') (then b')
         (where :pitch (logarithmic 65)))))

(comment
  (->> (quiet transcription-24-logarithmic)
       (with phrase-24)
       live/play)
)

(comment
  (mean-difference-squared
    (pitches transcription-24-raw)
    (pitches transcription-24-logarithmic))

  (mean-difference-squared
    (pitches transcription-24-raw)
    (pitches transcription-24-linear))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keytar             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn midi->linear [midi]
  (let [x nil]
    (get
      [   1    x    x    x    x    x    x    x    x    x    x    x
          2    x    x    x    x    x    x    3    x    x    x    x
          4    x    x    x    5    x    x    6    x    x    7    x
          8    x    9    x   10    x   11   12   13    x   14   15
         16   17   18   19   20   21   22,  24,  26   27   28,  30
         32,  34,  36,  38,  40,  42,  44,, 48,, 52,  54,  56,, 60]
      midi)))







(defn midi->hfreq [midi]
  (let [c1-midi 24
        c1-freq 37.71
        c2-midi 36
        c2-freq 65.41]
    ;(some-> midi (- c2-midi) midi->linear (* c2-freq))
    (some-> midi (- c1-midi) midi->linear (* c1-freq))
))

(def midi->freq midi->hfreq)
;(def midi->freq midi->hz)

(def keytar-instrument corgan #_blorp)

(defonce notes-in-progress (atom {}))

(def keytar-note-on
  (on-event [:midi :note-on]
            (fn [{note :note velocity :velocity}]
              (let [unit-volume (+ 1/4 (* 3/4 (/ velocity 128)))
                    synth (some-> note midi->freq (/ 2) (keytar-instrument :dur 15 :vol (/ velocity 128)))]
                (swap! notes-in-progress assoc note synth)))
            ::midi-note-on))

(def keytar-note-off
  (on-event [:midi :note-off]
           (fn [{note :note}]
             (ctl (@notes-in-progress note) :gate 0))
           ::midi-note-off))

(def kit {:kick #(drums/kick2 66)
          :tick #(drums/closed-hat :t 0.01 :hi 20),
          :tock #(drums/open-hat :t 0.04 :lo 50 :hi 100)})

(defmethod live/play-note :beat [{drum :drum}]
    ((kit drum)))

(defmethod live/play-note :butcherbird [{n :bird seconds :duration}]
  ((butcherbirds n)))

(def birdloop
  [{:time 0 :duration 8 :bird 23 :part :butcherbird}
   {:time 0 :duration 8 :bird 24 :part :butcherbird}
   {:time 16 :duration 8 :bird 19 :part :butcherbird}])

(def drumloop
  (->> (rhythm
         (cycle [1/2 1/2 1/2 1/4 1/4 ]))
    (having :drum (cycle [:kick :tick :tick :tock :kick]))
    (all :part :beat)
    (take-while #(-> % :time (< 24)))
    (tempo (bpm 90))))

(def jamloop (with (times 2 drumloop) birdloop))

(comment
  (live/jam (var jamloop))
  (live/play jamloop)
)

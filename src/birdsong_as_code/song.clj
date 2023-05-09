(ns birdsong-as-code.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

; Instruments
(definst bass [freq 110 volume 1.0]
  (-> (saw freq)
      (* (env-gen (perc 0.1 0.4) :action FREE))
      (* volume)))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.01 0.8 0.1) (line:kr 1 0 dur) :action FREE))
      (* 1/4 volume)))

(defmethod live/play-note :bass [{hertz :pitch}] (bass hertz))
(defmethod live/play-note :default [{hertz :pitch seconds :duration}] (organ hertz seconds))

(def harmonic-series
  (->> 
    (range 8 17)
    (map (partial * 100))
    (phrase (repeat 1/4))
    (tempo (bpm 90))))

(defn -main []
  (live/play track))

(comment
  ; Loop the track, allowing live editing.
  (live/play harmonic-series)
  (live/stop) 
)

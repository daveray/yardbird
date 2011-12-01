(ns yardbird.giant-steps
  (:use [yardbird.util :only [midi-note-to-pitch]])
  (:require [clojure.string :as string]
            [clojure.java.io :as jio]
            [overtone.music.pitch :as pitch]))

; Notes for Coltrane's "Giant Steps" solos
;http://scholar.lib.vt.edu/theses/public/etd-61098-131249/materials/dmfetd.pdf

(def all-solos (map (comp keyword str) (repeat "gs") (range 1 10)))

(defn load-solo 
  "Given a solo version, e.g. :gs1, through :gs9, returns a vector of 
  vectors of notes. Each vector is a single chorus of the solo. Each note
  is an eigth-note. Rests are represented with nil. Notes are keywords 
  of the form :Ab2. They can be converted to midi number format using
  (overtone.core/note)
  "
  [version]
  (with-open [f (java.io.PushbackReader. 
                  (jio/reader (jio/resource (str "yardbird/giant_steps/" (name version)))))]
    (binding [*read-eval* false] 
      (read f))))

; the nasty code I used to massage the data from the paper

; (defn process [in] (->> (string/split in #"\n\n")
;   (map #(re-find #"(GS\d)\* (.+)\n(?s)(.+)" %))
;   (map rest)
;   (mapcat (fn [[id changes notes]] 
;             (map-indexed 
;               (fn [i chorus]
;                 [id i changes (->> (string/split chorus #"\s+")
;                               (map #(if (= "7" %) nil (keyword %)))
;                               vec)])
;               (string/split notes #"\n"))))
;   (reduce (fn [acc [id i changes chorus]]
;             (update-in acc [id i] #(concat (or % []) chorus))) {})))
;  
; (defn -main []
;   (let [raw (slurp (jio/resource "yardbird/giant_steps/raw.txt"))
;         processed (process raw)]
;     (doseq [[version data] processed]
;       (spit (str "resources/yardbird/giant_steps/" (.toLowerCase version)) 
;             (pr-str (reduce conj [] (map (comp vec data) (sort (keys data))))))))) 
;  (defn -main [& args]
;    (doseq [id all-solos]
;      (let [s (load-solo id)]
;        (spit (str "resources/yardbird/giant_steps/" (name id))
;                (pr-str (vec (for [c (load-solo id)]
;                (->> c
;                  (map pitch/note)
;                  (map #(if % (+ % 24 -2)))
;                  (map midi-note-to-pitch)
;                  vec)))
;          )))))
; 

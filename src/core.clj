(ns core
  (:require [scad-clj.model :as m]
            [scad-clj.scad :as s]))

(defn save []
  (->> [(m/union
         (m/cube 10 20 30)
         (m/sphere 10))]
       (apply s/write-scad)
       (spit "scad/a.scad")))

(save)

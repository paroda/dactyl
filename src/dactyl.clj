(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [mmul]]
            [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer [*fn* pi
                                    with-fn
                                    scale
                                    polygon
                                    cube cylinder sphere
                                    hull
                                    mirror rotate translate
                                    union difference cut
                                    extrude-linear project
                                    color]
             :as model]))

(def π Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 6)
(def ncols 7)

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 4))             ; or, change this for more precise tenting control

(def pinky-15u false)                   ; controls whether the outer column uses 1.5u keys
(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 4)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def extra-row false)                   ; adds an extra bottom row to the outer columns
(def inner-column true)                ; adds an extra inner column (two less rows than nrows)
(def thumb-style "default")                ; toggles between "default", "mini", and "cf" thumb cluster

(def column-style :standard)

(defn column-offset [column]
  (if inner-column
    (cond (<= column 1) [0 -2 0]
          (= column 3) [0 2.82 -4.5]
          (>= column 5) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0])
    (cond (= column 2) [0 2.82 -4.5]
          (>= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0])))

(def thumb-offsets [6 -3 7])

(def keyboard-z-offset 40)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 3)                   ; extra space between the base of keys; original= 2
(def extra-height 1)                  ; original= 0.5

(def wall-z-offset -10)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-x-offset 10)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-y-offset 8)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;; http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

                                        ; If you use Cherry MX or Gateron switches, this can be turned on.
                                        ; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? false)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))
(def extra-cornerrow (if extra-row lastrow cornerrow))
(def innercol-offset (if inner-column 1 0))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.15)
(def keyswitch-width 14.15)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- (+ plate-thickness 0.5) retention-tab-thickness))
(def mount-width (+ keyswitch-width 3.2))
(def mount-height (+ keyswitch-height 2.7))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 (+ plate-thickness 0.5))
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- (/ plate-thickness 2) 0.25)]))
        left-wall (->> (cube 1.8 (+ keyswitch-height 3) (+ plate-thickness 0.5))
                       (translate [(+ (/ 1.8 2) (/ keyswitch-width 2))
                                   0
                                   (- (/ plate-thickness 2) 0.25)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (when create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                     (translate [(+ (/ keyswitch-width 2.5)) 0 (- (/ retention-tab-hole-thickness 2) 0.5)]))
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (->>
      top-nub-pair
      (rotate (/ π 2) [0 0 1])))))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 sa-length
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;; Fill the keyholes instead of placing a a keycap over them
(def keyhole-fill (->> (cube keyswitch-height keyswitch-width plate-thickness)
                       (translate [0 0 (/ plate-thickness 2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range (+ innercol-offset 0) ncols))
(def rows (range 0 nrows))

(def innercolumn 0)
(def innerrows (range 0 (- nrows 2)))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn offset-for-column [col, row]
  (if (and pinky-15u
           (= col lastcol)
           (<= row last-15u-row)
           (>= row first-15u-row))
    4.7625
    0))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column, row) 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
           :orthographic placed-shape-ortho
           :fixed        placed-shape-fixed
           placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                         (and inner-column (not= row cornerrow)(= column 0))
                         (not= row lastrow))]
           (->> single-plate
                                        ;                (rotate (/ π 2) [0 0 1])
                (key-place column row)))))
(def caps
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (and (= column 0) (< row 3))
                               (and (.contains [1 2] column) (< row 4))
                               (.contains [3 4 5 6] column))]
                 (->> (sa-cap (if (and pinky-15u (= column lastcol) (not= row lastrow)) 1.5 1))
                      (key-place column row)))
               (list (key-place 0 0 (sa-cap 1))
                     (key-place 0 1 (sa-cap 1))
                     (key-place 0 2 (sa-cap 1))))))

(def caps-fill
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
                               (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                               (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                               (and inner-column (not= row cornerrow)(= column 0))
                               (not= row lastrow))]
                 (key-place column row keyhole-fill))
               (list (key-place 0 0 keyhole-fill)
                     (key-place 0 1 keyhole-fill)
                     (key-place 0 2 keyhole-fill)))))

                                        ;placement for the innermost column
(def key-holes-inner
  (when inner-column
    (apply union
           (for [row innerrows]
             (->> single-plate
                                        ;               (rotate (/ π 2) [0 0 1])
                  (key-place 0 row))))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 4.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))

                                        ; wide posts for 1.5u keys in the main cluster
(if pinky-15u
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
      (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
      (def wide-post-tl web-post-tl)
      (def wide-post-bl web-post-bl)
      (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range (+ innercol-offset 0) (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

(def inner-connectors
  (when inner-column
    (apply union
           (concat
            ;; Row connections
            (for [column (range 0 1)
                  row (range 0 (- nrows 2))]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))

            ;; Column connections
            (for [row (range 0 (dec cornerrow))]
              (triangle-hulls
               (key-place innercolumn row web-post-bl)
               (key-place innercolumn row web-post-br)
               (key-place innercolumn (inc row) web-post-tl)
               (key-place innercolumn (inc row) web-post-tr)))

            ;; Diagonal connections
            (for [column (range 0 (dec ncols))
                  row (range 0 2)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))))))

(def extra-connectors
  (when extra-row
    (apply union
           (concat
            (for [column (range 3 ncols)
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-bl)
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tl)
               (key-place column (inc row) web-post-tr)))

            (for [column (range 3 (dec ncols))
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))

            (for [column (range 4 (dec ncols))
                  row (range lastrow nrows)]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))))))

;;;;;;;;;;;;;;;;;;;
;; Default Thumb ;;
;;;;;;;;;;;;;;;;;;;

(def thumborigin
  (map + (key-position (+ innercol-offset 1) cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

(defn thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-12 -16 3])
       ))
(defn thumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-32 -15 -2])))
(defn thumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  48) [0 0 1])
       (translate thumborigin)
       (translate [-29 -40 -13])
       ))
(defn thumb-ml-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  40) [0 0 1])
       (translate thumborigin)
       (translate [-51 -25 -12])))
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad -16) [1 0 0])
       (rotate (deg2rad -33) [0 1 0])
       (rotate (deg2rad  54) [0 0 1])
       (translate thumborigin)
       (translate [-37.8 -55.3 -25.3])
       ))
(defn thumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad  -4) [1 0 0])
       (rotate (deg2rad -35) [0 1 0])
       (rotate (deg2rad  52) [0 0 1])
       (translate thumborigin)
       (translate [-56.3 -43.3 -23.5])
       ))

(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-ml-place shape)
   (thumb-br-place shape)
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)
   (thumb-tl-place shape)))

(def larger-plate
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        ]
    (union top-plate (mirror [0 1 0] top-plate))))

(def larger-plate-half
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        ]
    (union top-plate (mirror [0 0 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

(def thumbcaps-fill
  (union
   (thumb-1x-layout keyhole-fill)
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def thumb
  (union
   (thumb-1x-layout (rotate (/ π 2) [0 0 0] single-plate))
   (thumb-tr-place (rotate (/ π 2) [0 0 1] single-plate))
   (thumb-tr-place larger-plate)
   (thumb-tl-place (rotate (/ π 2) [0 0 1] single-plate))
   (thumb-tl-place larger-plate-half)))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  1.1) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  1.1) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -1.1) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -1.1) post-adj) 0] web-post))

(def thumb-connectors
  (union
   (triangle-hulls    ; top two
    (thumb-tl-place thumb-post-tr)
    (thumb-tl-place (translate [-0.33 -0.25 0] web-post-br))
    (thumb-tr-place thumb-post-tl)
    (thumb-tr-place thumb-post-bl))
   (triangle-hulls    ; bottom two on the right
    (thumb-br-place web-post-tr)
    (thumb-br-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-mr-place web-post-bl))
   (triangle-hulls    ; bottom two on the left
    (thumb-bl-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-ml-place web-post-tl)
    (thumb-ml-place web-post-bl))
   (triangle-hulls    ; centers of the bottom four
    (thumb-br-place web-post-tl)
    (thumb-bl-place web-post-bl)
    (thumb-br-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-ml-place web-post-bl)
    (thumb-mr-place web-post-tr)
    (thumb-ml-place web-post-br))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (thumb-tl-place thumb-post-tl)
    (thumb-ml-place web-post-tr)
    (thumb-tl-place (translate [0.25 0.1 0] web-post-bl))
    (thumb-ml-place web-post-br)
    (thumb-tl-place (translate [-0.33 -0.25 0] web-post-br))
    (thumb-mr-place web-post-tr)
    (thumb-tr-place thumb-post-bl)
    (thumb-mr-place web-post-br)
    (thumb-tr-place thumb-post-br))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (thumb-tl-place thumb-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (thumb-tl-place thumb-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (thumb-tr-place thumb-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (thumb-tr-place thumb-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-tr-place thumb-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-tr-place thumb-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl)
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))))

;;;;;;;;;;;;;;;;
;; Mini Thumb ;;
;;;;;;;;;;;;;;;;

(defn minithumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  14) [1 0 0])
       (rotate (deg2rad -15) [0 1 0])
       (rotate (deg2rad  10) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-15 -10 5]))) ; original 1.5u  (translate [-12 -16 3])
(defn minithumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  25) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-35 -16 -2]))) ; original 1.5u (translate [-32 -15 -2])))
(defn minithumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  25) [0 0 1])
       (translate thumborigin)
       (translate [-23 -34 -6])))
(defn minithumb-br-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  35) [0 0 1])
       (translate thumborigin)
       (translate [-39 -43 -16])))
(defn minithumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -32) [0 1 0])
       (rotate (deg2rad  35) [0 0 1])
       (translate thumborigin)
       (translate [-51 -25 -11.5]))) ;        (translate [-51 -25 -12])))

(defn minithumb-1x-layout [shape]
  (union
   (minithumb-mr-place shape)
   (minithumb-br-place shape)
   (minithumb-tl-place shape)
   (minithumb-bl-place shape)))

(defn minithumb-15x-layout [shape]
  (union
   (minithumb-tr-place shape)))

(def minithumbcaps
  (union
   (minithumb-1x-layout (sa-cap 1))
   (minithumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1)))))

(def minithumbcaps-fill
  (union
   (minithumb-1x-layout keyhole-fill)
   (minithumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def minithumb
  (union
   (minithumb-1x-layout single-plate)
   (minithumb-15x-layout single-plate)))

(def minithumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def minithumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def minithumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def minithumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def minithumb-connectors
  (union
   (triangle-hulls    ; top two
    (minithumb-tl-place web-post-tr)
    (minithumb-tl-place web-post-br)
    (minithumb-tr-place minithumb-post-tl)
    (minithumb-tr-place minithumb-post-bl))
   (triangle-hulls    ; bottom two
    (minithumb-br-place web-post-tr)
    (minithumb-br-place web-post-br)
    (minithumb-mr-place web-post-tl)
    (minithumb-mr-place web-post-bl))
   (triangle-hulls
    (minithumb-mr-place web-post-tr)
    (minithumb-mr-place web-post-br)
    (minithumb-tr-place minithumb-post-br))
   (triangle-hulls    ; between top row and bottom row
    (minithumb-br-place web-post-tl)
    (minithumb-bl-place web-post-bl)
    (minithumb-br-place web-post-tr)
    (minithumb-bl-place web-post-br)
    (minithumb-mr-place web-post-tl)
    (minithumb-tl-place web-post-bl)
    (minithumb-mr-place web-post-tr)
    (minithumb-tl-place web-post-br)
    (minithumb-tr-place web-post-bl)
    (minithumb-mr-place web-post-tr)
    (minithumb-tr-place web-post-br))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (minithumb-tl-place web-post-tl)
    (minithumb-bl-place web-post-tr)
    (minithumb-tl-place web-post-bl)
    (minithumb-bl-place web-post-br)
    (minithumb-mr-place web-post-tr)
    (minithumb-tl-place web-post-bl)
    (minithumb-tl-place web-post-br)
    (minithumb-mr-place web-post-tr))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (minithumb-tl-place web-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (minithumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (minithumb-tr-place minithumb-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (minithumb-tr-place minithumb-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (minithumb-tr-place minithumb-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (minithumb-tr-place minithumb-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl)
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br)
    )
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))))

;;;;;;;;;;;;;;;;
;; cf Thumb ;;
;;;;;;;;;;;;;;;;

(defn cfthumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -24) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-13 -9.8 4])))
(defn cfthumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  6) [1 0 0])
       (rotate (deg2rad -24) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-7.5 -29.5 0])))
(defn cfthumb-ml-place [shape]
  (->> shape
       (rotate (deg2rad  8) [1 0 0])
       (rotate (deg2rad -31) [0 1 0])
       (rotate (deg2rad  14) [0 0 1])
       (translate thumborigin)
       (translate [-30.5 -17 -6])))
(defn cfthumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  4) [1 0 0])
       (rotate (deg2rad -31) [0 1 0])
       (rotate (deg2rad  14) [0 0 1])
       (translate thumborigin)
       (translate [-22.2 -41 -10.3])))
(defn cfthumb-br-place [shape]
  (->> shape
       (rotate (deg2rad   2) [1 0 0])
       (rotate (deg2rad -37) [0 1 0])
       (rotate (deg2rad  18) [0 0 1])
       (translate thumborigin)
       (translate [-37 -46.4 -22])))
(defn cfthumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -37) [0 1 0])
       (rotate (deg2rad  18) [0 0 1])
       (translate thumborigin)
       (translate [-47 -23 -19])))

(defn cfthumb-1x-layout [shape]
  (union
   (cfthumb-tr-place (rotate (/ π 2) [0 0 0] shape))
   (cfthumb-mr-place shape)
   (cfthumb-br-place shape)
   (cfthumb-tl-place (rotate (/ π 2) [0 0 0] shape))))

(defn cfthumb-15x-layout [shape]
  (union
   (cfthumb-bl-place shape)
   (cfthumb-ml-place shape)))

(def cfthumbcaps
  (union
   (cfthumb-1x-layout (sa-cap 1))
   (cfthumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

(def cfthumbcaps-fill
  (union
   (cfthumb-1x-layout keyhole-fill)
   (cfthumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def cfthumb
  (union
   (cfthumb-1x-layout single-plate)
   (cfthumb-15x-layout larger-plate-half)
   (cfthumb-15x-layout single-plate)))

(def cfthumb-connectors
  (union
   (triangle-hulls    ; top two
    (cfthumb-tl-place web-post-tl)
    (cfthumb-tl-place web-post-bl)
    (cfthumb-ml-place thumb-post-tr)
    (cfthumb-ml-place web-post-br))
   (triangle-hulls
    (cfthumb-ml-place thumb-post-tl)
    (cfthumb-ml-place web-post-bl)
    (cfthumb-bl-place thumb-post-tr)
    (cfthumb-bl-place web-post-br))
   (triangle-hulls    ; bottom two
    (cfthumb-br-place web-post-tr)
    (cfthumb-br-place web-post-br)
    (cfthumb-mr-place web-post-tl)
    (cfthumb-mr-place web-post-bl))
   (triangle-hulls
    (cfthumb-mr-place web-post-tr)
    (cfthumb-mr-place web-post-br)
    (cfthumb-tr-place web-post-tl)
    (cfthumb-tr-place web-post-bl))
   (triangle-hulls
    (cfthumb-tr-place web-post-br)
    (cfthumb-tr-place web-post-bl)
    (cfthumb-mr-place web-post-br))
   (triangle-hulls    ; between top row and bottom row
    (cfthumb-br-place web-post-tl)
    (cfthumb-bl-place web-post-bl)
    (cfthumb-br-place web-post-tr)
    (cfthumb-bl-place web-post-br)
    (cfthumb-mr-place web-post-tl)
    (cfthumb-ml-place web-post-bl)
    (cfthumb-mr-place web-post-tr)
    (cfthumb-ml-place web-post-br)
    (cfthumb-tr-place web-post-tl)
    (cfthumb-tl-place web-post-bl)
    (cfthumb-tr-place web-post-tr)
    (cfthumb-tl-place web-post-br))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (cfthumb-ml-place thumb-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (cfthumb-ml-place thumb-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (cfthumb-tl-place web-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (cfthumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (cfthumb-tl-place web-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (cfthumb-tl-place web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (cfthumb-tl-place web-post-br)
    (cfthumb-tr-place web-post-tr))
   (triangle-hulls
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (triangle-hulls
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) lastrow web-post-bl))
   (triangle-hulls
    (cfthumb-tr-place web-post-br)
    (cfthumb-tr-place web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-bl))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))))

                                        ;switching connectors, switchplates, etc. depending on thumb-style used
(when (= thumb-style "default")
  (def thumb-type thumb)
  (def thumb-connector-type thumb-connectors)
  (def thumbcaps-type thumbcaps)
  (def thumbcaps-fill-type thumbcaps-fill))

(when (= thumb-style "cf")
  (def thumb-type cfthumb)
  (def thumb-connector-type cfthumb-connectors)
  (def thumbcaps-type cfthumbcaps)
  (def thumbcaps-fill-type cfthumbcaps-fill))

(when (= thumb-style "mini")
  (def thumb-type minithumb)
  (def thumb-connector-type minithumb-connectors)
  (def thumbcaps-type minithumbcaps)
  (def thumbcaps-fill-type minithumbcaps-fill))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 16)
(def left-wall-z-offset 0)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-x-offset) (* dy wall-y-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-x-offset wall-thickness)) (* dy (+ wall-y-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def right-wall
  (if pinky-15u
    (union
                                        ; corner between the right wall and back wall
     (if (> first-15u-row 0)
       (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
       (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)
              (key-wall-brace lastcol 0 0 1 wide-post-tr lastcol 0 1 0 wide-post-tr)))
                                        ; corner between the right wall and front wall
     (if (= last-15u-row extra-cornerrow)
       (union (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 0 -1 wide-post-br)
              (key-wall-brace lastcol extra-cornerrow 0 -1 wide-post-br lastcol extra-cornerrow 1 0 wide-post-br))
       (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br))

     (when (>= first-15u-row 2)
       (for [y (range 0 (dec first-15u-row))]
         (union (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br)
                (key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr))))

     (when (>= first-15u-row 1)
       (for [y (range (dec first-15u-row) first-15u-row)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol (inc y) 1 0 wide-post-tr)))

     (for [y (range first-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-tr lastcol y 1 0 wide-post-br))
     (for [y (range first-15u-row last-15u-row)] (key-wall-brace lastcol (inc y) 1 0 wide-post-tr lastcol y 1 0 wide-post-br))

     (when (<= last-15u-row (- extra-cornerrow 1))
       (for [y (range last-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-br lastcol (inc y) 1 0 web-post-br)))

     (when (<= last-15u-row (- extra-cornerrow 2))
       (for [y (range (inc last-15u-row) extra-cornerrow)]
         (union (key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr)
                (key-wall-brace lastcol (inc y) 1 0 web-post-tr lastcol (inc y) 1 0 web-post-br))))
     )
    (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
           (if extra-row
             (union (for [y (range 0 (inc lastrow))] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 (inc lastrow))] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
             (union (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
             )
           (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br)
           )))

(def cf-thumb-wall
  (union
                                        ; thumb walls
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-tr-place  0 -1 web-post-br)
   (wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-mr-place  0 -1.15 web-post-bl)
   (wall-brace cfthumb-br-place  0 -1 web-post-br cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -0.3  1 thumb-post-tr cfthumb-bl-place  0  1 thumb-post-tl)
   (wall-brace cfthumb-br-place -1  0 web-post-tl cfthumb-br-place -1  0 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place -1  0 web-post-bl)
                                        ; cfthumb corners
   (wall-brace cfthumb-br-place -1  0 web-post-bl cfthumb-br-place  0 -1 web-post-bl)
   (wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place  0  1 thumb-post-tl)
                                        ; cfthumb tweeners
   (wall-brace cfthumb-mr-place  0 -1.15 web-post-bl cfthumb-br-place  0 -1 web-post-br)
   (wall-brace cfthumb-bl-place -1  0 web-post-bl cfthumb-br-place -1  0 web-post-tl)
   (wall-brace cfthumb-tr-place  0 -1 web-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
                                        ; clunky bit on the top left cfthumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr)))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 (- cornerrow innercol-offset) web-post-bl)
    (cfthumb-ml-place thumb-post-tl))
   (hull
    (cfthumb-bl-place thumb-post-tr)
    (cfthumb-bl-place (translate (wall-locate1 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
    (cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
    (cfthumb-ml-place thumb-post-tl))
                                        ; connectors below the inner column to the thumb & second column
   (when inner-column
     (union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 1 cornerrow web-post-bl)
       (cfthumb-ml-place thumb-post-tl))))))

(def mini-thumb-wall
  (union
                                        ; thumb walls
   (wall-brace minithumb-mr-place  0 -1 web-post-br minithumb-tr-place  0 -1 minithumb-post-br)
   (wall-brace minithumb-mr-place  0 -1 web-post-br minithumb-mr-place  0 -1 web-post-bl)
   (wall-brace minithumb-br-place  0 -1 web-post-br minithumb-br-place  0 -1 web-post-bl)
   (wall-brace minithumb-bl-place  0  1 web-post-tr minithumb-bl-place  0  1 web-post-tl)
   (wall-brace minithumb-br-place -1  0 web-post-tl minithumb-br-place -1  0 web-post-bl)
   (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place -1  0 web-post-bl)
                                        ; minithumb corners
   (wall-brace minithumb-br-place -1  0 web-post-bl minithumb-br-place  0 -1 web-post-bl)
   (wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place  0  1 web-post-tl)
                                        ; minithumb tweeners
   (wall-brace minithumb-mr-place  0 -1 web-post-bl minithumb-br-place  0 -1 web-post-br)
   (wall-brace minithumb-bl-place -1  0 web-post-bl minithumb-br-place -1  0 web-post-tl)
   (wall-brace minithumb-tr-place  0 -1 minithumb-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
                                        ; clunky bit on the top left minithumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (minithumb-bl-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (minithumb-bl-place (translate (wall-locate3 -0.3 1) web-post-tr)))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (minithumb-bl-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (minithumb-bl-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (minithumb-tl-place web-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (minithumb-tl-place web-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 (- cornerrow innercol-offset) web-post-bl)
    (minithumb-tl-place web-post-tl))
   (hull
    (minithumb-bl-place web-post-tr)
    (minithumb-bl-place (translate (wall-locate1 -0.3 1) web-post-tr))
    (minithumb-bl-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (minithumb-bl-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (minithumb-tl-place web-post-tl))
                                        ; connectors below the inner column to the thumb & second column
   (when inner-column
     (union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 1 cornerrow web-post-bl)
       (minithumb-tl-place minithumb-post-tl))))))

(def default-thumb-wall
  (union
                                        ; thumb walls
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-tr-place  0 -1 thumb-post-br)
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-mr-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-ml-place -0.3  1 web-post-tr thumb-ml-place  0  1 web-post-tl)
   (wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
   (wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)
                                        ; thumb corners
   (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)
                                        ; thumb tweeners
   (wall-brace thumb-mr-place  0 -1 web-post-bl thumb-br-place  0 -1 web-post-br)
   (wall-brace thumb-ml-place  0  1 web-post-tl thumb-bl-place  0  1 web-post-tr)
   (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
   (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
                                        ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr)))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (thumb-tl-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-tl-place thumb-post-tl))
   (hull
    (left-key-place (- cornerrow innercol-offset) -1 web-post)
    (left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 (- cornerrow innercol-offset) web-post-bl)
    (key-place 0 (- cornerrow innercol-offset) (translate (wall-locate1 0 0) web-post-bl))
    (thumb-tl-place thumb-post-tl))
                                        ; connectors below the inner column to the thumb & second column
   (when inner-column
     (union
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 (dec cornerrow) web-post-br)
       (key-place 0 cornerrow web-post-tr))
      (hull
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-tl)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 0 cornerrow web-post-tr)
       (key-place 1 cornerrow web-post-bl))
      (hull
       (key-place 0 (dec cornerrow) web-post-bl)
       (key-place 1 cornerrow web-post-bl)
       (thumb-tl-place thumb-post-tl))))
   (hull
    (thumb-ml-place web-post-tr)
    (thumb-ml-place (translate (wall-locate1 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (thumb-tl-place thumb-post-tl))))

                                        ;switching walls depending on thumb-style used
(def thumb-wall-type
  (case thumb-style
    "default" default-thumb-wall
    "cf" cf-thumb-wall
    "mini" mini-thumb-wall))

(def case-walls
  (union
   thumb-wall-type
   right-wall
                                        ; back wall
   (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
                                        ; left wall
   (for [y (range 0 (- lastrow innercol-offset))] (union (wall-brace (partial left-key-place y 1) -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
                                                         (hull (key-place 0 y web-post-tl)
                                                               (key-place 0 y web-post-bl)
                                                               (left-key-place y  1 web-post)
                                                               (left-key-place y -1 web-post))))
   (for [y (range 1 (- lastrow innercol-offset))] (union
                                                   (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
                                                   (hull (key-place 0 y       web-post-tl)
                                                         (key-place 0 (dec y) web-post-bl)
                                                         (left-key-place y        1 web-post)
                                                         (left-key-place (dec y) -1 web-post)
                                                         )))
   (wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) -0.6 1 web-post)
   (wall-brace (partial left-key-place 0 1) -0.6 1 web-post (partial left-key-place 0 1) -1 0 web-post)
                                        ; front wall
   (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-bl (+ innercol-offset 3) lastrow   0 -1 web-post-br)
   (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-br (+ innercol-offset 4) extra-cornerrow 0 -1 web-post-bl)
   (for [x (range (+ innercol-offset 4) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl x       extra-cornerrow 0 -1 web-post-br))
   (for [x (range (+ innercol-offset 5) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl (dec x) extra-cornerrow 0 -1 web-post-br))
   ))

                                        ; Offsets for the controller/trrs holder cutout
(def holder-offset
  (case nrows
    4 -3.5
    5 0
    6 (if inner-column
        3.2
        2.2)))

(def notch-offset
  (case nrows
    4 3.35
    5 0.15
    6 -5.07))

                                        ; Cutout for controller/trrs jack holder
(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))
(def usb-holder-position (map + [(+ 18.8 holder-offset) 18.7 1.3] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-holder-space  (translate (map + usb-holder-position [-1.5 (* -1 wall-thickness) 2.9]) (cube 28.666 30 12.4)))
(def usb-holder-notch  (translate (map + usb-holder-position [-1.5 (+ 4.4 notch-offset) 2.9]) (cube 31.366 1.3 12.4)))
(def trrs-notch        (translate (map + usb-holder-position [-10.33 (+ 3.6 notch-offset) 6.6]) (cube 8.4 2.4 19.8)))

                                        ; Screw insert definition & position
(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
          (cylinder [bottom-radius top-radius] height)))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                          (if shift-down  (key-position column row (map - (wall-locate2  0 -2.5) [0 (/ mount-height 2) 0]))
                              (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                  (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

                                        ; Offsets for the screw inserts dependent on extra-row & pinky-15u
(when (and pinky-15u extra-row)
  (def screw-offset-tr [1 7 0])
  (def screw-offset-br [7 14 0]))
(when (and pinky-15u (false? extra-row))
  (def screw-offset-tr [1 7 0])
  (def screw-offset-br [6.5 15.5 0]))
(when (and (false? pinky-15u) extra-row)
  (def screw-offset-tr [-3.5 6.5 0])
  (def screw-offset-br [-3.5 -6.5 0]))
(when (and (false? pinky-15u) (false? extra-row))
  (def screw-offset-tr [-4 6.5 0])
  (def screw-offset-br [-6 13 0]))

                                        ; Offsets for the screw inserts dependent on thumb-style & inner-column
(when (and (= thumb-style "cf") inner-column)
  (def screw-offset-bl [9 4 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "cf") (false? inner-column))
  (def screw-offset-bl [-7.7 2 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [13 -7 0]))
(when (and (= thumb-style "mini") inner-column)
  (def screw-offset-bl [14 8 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [-1 -7 0]))
(when (and (= thumb-style "mini") (false? inner-column))
  (def screw-offset-bl [-1 4.2 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [-1 -7 0]))
(when (and (= thumb-style "default") inner-column)
  (def screw-offset-bl [5 -6 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [8 -1 0]))
(when (and (= thumb-style "default") (false? inner-column))
  (def screw-offset-bl [-11.7 -8 0])
  (def screw-offset-tm [9.5 -4.5 0])
  (def screw-offset-bm [8 -1 0]))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 0         bottom-radius top-radius height [8 10.5 0])
         (screw-insert 0 lastrow   bottom-radius top-radius height screw-offset-bl)
         (screw-insert lastcol lastrow  bottom-radius top-radius height screw-offset-br)
         (screw-insert lastcol 0         bottom-radius top-radius height screw-offset-tr)
         (screw-insert (+ 2 innercol-offset) 0         bottom-radius top-radius height screw-offset-tm)
         (screw-insert (+ 1 innercol-offset) lastrow         bottom-radius top-radius height screw-offset-bm)))

                                        ; Hole Depth Y: 4.4
(def screw-insert-height 6)

                                        ; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

                                        ; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

                                        ; Connectors between outer column and right wall when 1.5u keys are used
(def pinky-connectors
  (when pinky-15u
    (apply union
           (concat
            ;; Row connections
            (for [row (range first-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol row web-post-tr)
               (key-place lastcol row wide-post-tr)
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)))
            (when-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
                                                         (triangle-hulls
                                                          (key-place lastcol (inc row) web-post-tr)
                                                          (key-place lastcol row wide-post-br)
                                                          (key-place lastcol (inc row) web-post-br))))
            (when-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
                                            (triangle-hulls
                                             (key-place lastcol row web-post-tr)
                                             (key-place lastcol (inc row) wide-post-tr)
                                             (key-place lastcol row web-post-br))))

            ;; Column connections
            (for [row (range first-15u-row last-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-tr)
               (key-place lastcol (inc row) wide-post-tr)))
            (when-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
                                                         (triangle-hulls
                                                          (key-place lastcol row web-post-br)
                                                          (key-place lastcol row wide-post-br)
                                                          (key-place lastcol (inc row) web-post-tr))))
            (when-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
                                            (triangle-hulls
                                             (key-place lastcol row web-post-br)
                                             (key-place lastcol (inc row) wide-post-tr)
                                             (key-place lastcol (inc row) web-post-tr))))
            ))))

;;; WRIST PAD

(let [ps1 [[0 85]
           [10 87]
           [18 90]
           [20 91]
           [30 91]
           [34 90]
           [40 87]
           [49 80]
           [55 70]
           [57 60]
           [56 50]
           [52 40]
           [50 37]
           [44 30]
           [40 27]
           [30 20]
           [20 16]
           [10 14]
           [0 13]]
      ps2 [[0 13]
           [-10 14]
           [-20 16]
           [-30 20]
           [-40 26]
           [-44 30]
           [-50 36]
           [-53 40]
           [-57 50]
           [-58 60]
           [-57 70]
           [-52 80]
           [-50 80]
           [-38 90]
           [-30 92]
           [-20 91]
           [-17 90]
           [-10 87]
           [0 85]]
      h 35
      plate (->>
             (union
              (for [ps [ps1 ps2]]
                (hull
                 (for [[x y] ps]
                   (translate [x y (* h 0.5)] (cylinder 0.1 0.1))))))
             (rotate (/ π 16) [0 1 0])
             (rotate (/ π 32) [1 0 0]))
      right (hull
             plate
             (extrude-linear {:height 1, :center false} (project plate)))
      left (mirror [1 0 0] right)]
  (spit "things/wrist-pad-right.scad" (write-scad right))
  (spit "things/wrist-pad-left.scad" (write-scad left)))

;;; Case roof (just key holes)

(def case-roof-right (union
                      key-holes
                      key-holes-inner
                      pinky-connectors
                      extra-connectors
                      connectors
                      inner-connectors
                      thumb-type
                      thumb-connector-type))

(spit "things/case-roof-right.scad" (write-scad case-roof-right))

;;; Case sockets section
(def case-socket-walls
  (let [x1 -80 x2 10 x3 -72 x4 -78 x5 -45
        y1 80 y2 10 y3 38 y4 75 y5 65 y6 23
        z1 0 z2 100 z3 60 z4 90 z5 100
        p1 (translate [x1 y2 z1] (sphere 2))
        p2 (translate [x1 y2 z2] (sphere 2))
        p3 (translate [x2 y1 z1] (sphere 2))
        p4 (translate [x2 y1 z3] (sphere 2))
        p5 (translate [x3 y1 z1] (sphere 2))
        p6 (translate [x3 y1 z2] (sphere 2))
        p7 (translate [x3 y3 z1] (sphere 2))
        p8 (translate [x3 y3 z2] (sphere 2))
        p9 (translate [x2 y4 z1] (sphere 2))
        p10 (translate [x2 y4 z3] (sphere 2))
        p11 (translate [x4 y2 z1] (sphere 2))
        p12 (translate [x4 y2 z2] (sphere 2))
        p13 (translate [x1 y6 z1] (sphere 2))
        p14 (translate [x1 y6 z2] (sphere 2))
        p30 (translate [x5 y5 z5] (sphere 2))]
    (union (hull p3 p4 p5 p6)
           (hull p5 p6 p7 p8)
           (hull p7 p8 p13 p14)
           (hull p13 p14 p1 p2)
           (hull p1 p2 p11 p12)
           (hull p3 p4 p9 p10)
           (hull p4 p10 p30)
           (hull p4 p6 p30)
           (hull p8 p6 p30)
           (hull p8 p14 p30)
           (hull p2 p14 p30)
           (hull p2 p12 p30))))

(def case-socket-holes-common
  (union (->> (scale [0.1 0.1 1] (cylinder 65 20))
              (rotate (/ π 2) [0 1 0])
              (translate [-80 33 50]))
         (->> (scale [0.1 0.1 1] (cylinder 27 20))
              (rotate (/ π 2) [0 1 0])
              (translate [-80 48.5 50]))
         (translate [-80 17 50] (cube 15 4 20))))

(def case-socket-holes-right
  (union (translate [-44 80 21] (cube 18 20 14))
         (translate [-21 80 18] (cube 12 20 8))))

(def case-socket-common (difference case-socket-walls
                                    case-socket-holes-common
                                    (translate [0 0 -20] (cube 350 350 40))))

(def case-socket-right (difference case-socket-common
                                   case-socket-holes-right))

(spit "things/case-socket-right.scad" (write-scad case-socket-right))

(def case-socket-holes-left
  (union (translate [17 80 21] (cube 18 20 14))
         (translate [40 80 18] (cube 12 20 8))))

(def case-socket-left (difference (mirror [1 0 0] case-socket-common)
                                  case-socket-holes-left))

(spit "things/case-socket-left.scad" (write-scad case-socket-left))

;;; aux

(def screw-hole-3mm (scale [0.1 0.1 0.1]
                           (union (cylinder 15 300)
                                  (cylinder 30 100))))

(def screw-hole-3mm-base (scale [0.1 0.1 0.1]
                                (union (cylinder 10 200)
                                       (difference (cylinder 50 160)
                                                   (translate [0 0 -60] (cube 200 200 160))))))

;;; case panel section

(def case-panel
  (let [h 30
        full? true]
    (union
     (translate [-95 -37 (+ 50 h)]
                (rotate [(* π 0.2) 0 (* π -0.14)] nil
                        (difference
                         (hull (translate [-23 -28 0] (sphere 2))
                               (translate [23 -28 0] (sphere 2))
                               (translate [-23 28 0] (sphere 2))
                               (translate [23 28 0] (sphere 2)))
                         (cube 36 36 10))))
     (let [x1 -106.1 x2 -64.4 x3 -80 x4 -125.7 x5 -83.6
           y1 -6.4 y2 -26.1 y3 5 y4 -47.9 y5 -67.5
           z1 (+ h 66.54) z2 100 z3 (+ h 33) z4 (if full? -10 h)
           p1 (translate [x1 y1 z1] (sphere 2))
           p2 (translate [x2 y2 z1] (sphere 2))
           p3 (translate [x3 y3 z2] (sphere 2))
           p4 (translate [x1 y3 z1] (sphere 2))
           p5 (translate [x4 y4 z3] (sphere 2))
           p6 (translate [x5 y5 z3] (sphere 2))
           p01 (translate [x1 y1 z4] (sphere 2))
           p02 (translate [x2 y2 z4] (sphere 2))
           p03 (translate [x3 y3 z4] (sphere 2))
           p04 (translate [x1 y3 z4] (sphere 2))
           p05 (translate [x4 y4 z4] (sphere 2))
           p06 (translate [x5 y5 z4] (sphere 2))]
       (union (hull p1 p2 p3)
              (hull p1 p3 p4)
              (hull p4 p3 p04 p03)
              (hull p1 p4 p5)
              (hull p4 p5 p04 p05)
              (hull p5 p6 p05 p06)
              (hull p6 p2 p06 p02)
              (hull p01 p02 p03 p04 p05 p06))))))

;;; case walls

(def case-walls-right
  (let [place #(translate [-59.8 60 107] (rotate [(/ π 2) 0 (* π 0.26)] nil %))]
    (difference (union (difference case-walls
                                   (hull case-panel))
                       case-panel
                       (place screw-hole-3mm-base))
                case-roof-right
                (hull case-socket-right)
                (place screw-hole-3mm)
                (translate [0 0 -20] (cube 350 350 40)))))

(spit "things/case-walls-right.scad" (write-scad case-walls-right))

;; base plate

(def plate-right
  (->> (union case-walls
              case-socket-right
              (map hull (rest key-holes))
              (map hull (rest key-holes-inner))
              pinky-connectors
              extra-connectors
              connectors
              inner-connectors
              (map hull (rest thumb-type))
              thumb-connector-type
              (hull case-panel))
       (translate [0 0 -0.1])
       (model/project)
       (model/extrude-linear {:height 3})))

(def screw-hole-3mm-2 (scale [0.1 0.1 0.1]
                             (union (cylinder 15 300)
                                    (cylinder 30 60)
                                    (translate [0 0 30] (sphere 30)))))

(spit "things/plate-right.scad"
      (letfn [(cut-hole [[x y] m]
                (difference
                 (union m
                        (translate [x y -3.5] screw-hole-3mm-base))
                 (translate [x y -3.5] screw-hole-3mm-2)))]
        (->> (union plate-right
                    #_(let [x -43 y -96]
                        (difference (translate [x y 0] (cylinder 10.5 30))
                                    (translate [x y 0] (cylinder 6 60)))))
             (cut-hole [42 -42])
             (cut-hole [21.8 70.3])
             (cut-hole [-63.5 71.9])
             (cut-hole [-87 -3])
             (cut-hole [-110 -45.6])
             (cut-hole [-43 -96])
             write-scad)))


;;; Boards test

(def satelite-board
  (union (translate [1 40 10] (cube 2 80 20))
         (translate [21 63 -5] (cube 38 22 10))
         (translate [-11 22 10] (cube 22 3 18))
         (->> (cylinder 5.5 15)
              (rotate (/ π 2) [0 1 0])
              (translate [-7.5 37 10]))
         (->> (cylinder 2.5 15)
              (rotate (/ π 2) [0 1 0])
              (translate [-7.5 52.5 10]))))

(spit "things/satelite-board.scad" (write-scad satelite-board))

(def primary-board
  (union (translate [25 37.5 1] (cube 50 75 2))
         (translate [16 73 9] (cube 16 10 14))
         (translate [39 70 5] (cube 8 10 6))))

(spit "things/primary-board.scad" (write-scad primary-board))

(spit "things/test-board-in-case.scad"
      (write-scad
       (union case-walls-right
              (translate [-60 0 10]
                         (union
                          primary-board
                          (translate [0 -5 30] satelite-board)))
              case-socket-right)))

;;; key foot plate

(def key-foot-plate
  (scale [0.1 0.1 0.1]
         (difference (union (translate [0 0 10] (cube 160 160 20))
                            (translate [50 55 20] (cube 20 10 40))
                            (translate [50  0 20] (cube 20 10 40))
                            (translate [-30 75 20] (cube 20 10 40))
                            (translate [-30 20 20] (cube 20 10 40))
                            (translate [-75 -35 20] (cube 10 20 40))
                            (translate [75 -35 20] (cube 10 20 40))
                            (translate [0 -35 20] (cube 130 20 40)))
                     (cylinder 22 200)
                     (translate [-25.4 50.8 0] (cylinder 16.5 200))
                     (translate [38.1 25.4 0] (cylinder 16.5 200))
                     (translate [80 80 0] (cylinder 20 200))
                     (translate [80 -80 0] (cylinder 20 200))
                     (translate [-80 80 0] (cylinder 20 200))
                     (translate [-80 -80 0] (cylinder 20 200)))))

(spit "things/key-foot-plate.scad"
      (write-scad
       (apply union
              (translate [50 50 0.1] (cube 120 120 0.2))
              (for [i (range 6)
                    j (range 6)]
                (translate [(* 20 i) (* 20 j) 0] key-foot-plate)))))

;;; ball socket

(def ball-socket
  (let [s 10 S (/ 1 s)
        w 42.0 H 24.0
        d0 34.0
        d1 6.0 l1 2.5
        d2 3.0 l2 6.0
        w1 23.0 h1 29.0 h1i 26.0
        theta (* 20 (/ π 180))
        L (/ (+ d0 d1) 2)
        r (* L (Math/cos theta))
        h (- (/ d0 2) (* L (Math/sin theta)))
        ri (- r 6)
        ro (/ w 2)]
    (println {:L L :r r :h h :ri ri :ro ro})
    (scale [S S S]
           (let [rod (rotate (/ π 2) [1 0 0] (cylinder (* s (/ (+ 0.5 d2) 2)) (* s l2)))
                 cut1 (union (cube (* s (+ w 1)) (* s (+ l1 0.5)) (* s (+ d1 1)))
                             (translate [(* s r) 0 0] rod)
                             (translate [(* s r -1) 0 0] rod))
                 cut2 (rotate (/ π 3) [0 0 1] cut1)
                 cut3 (rotate (/ π 3) [0 0 1] cut2)
                 cut4 (translate [0 0 (* s (- 0 h (/ H 2)))]
                                 (cube (* s w1) (* s h1) (* s H)))
                 cut5 (union (translate [0 (* s (+ 0 (/ h1 2) -2)) (* s (- 1 h (/ H 2)))]
                                        (cylinder (* s 4) (* s H)))
                             (translate [0 (* s (- 0 (/ h1 2) -2)) (* s (- 1 h (/ H 2)))]
                                        (cylinder (* s 4) (* s H))))
                 cut6 (let [z (- 0 h 5 (/ H 2))]
                        (println "cut6" {:z z})
                        (translate [0 0 (* s z)]
                                   (cube (* s (+ w 1)) (* s h1i) (* s H))))]
             (difference
              (union
               #_(translate [0 0 (* s L (Math/sin theta))] (sphere (* s (/ d0 2))))
               #_(translate [(* s r) 0 0]
                            (rotate (/ π 2) [1 0 0] (cylinder (* s (/ d1 2)) (* s l1))))
               (difference
                (union
                 (let [z (- (+ 1 (/ d2 2)) (/ H 2))]
                   (println "cyl1" {:z z})
                   (difference
                    (translate [0 0 (* s z)]
                               (difference (cylinder (* s ro) (* s H))
                                           (cylinder (* s ri) (* s (* 2 H)))))
                    (translate [0 0 (* s L (Math/sin theta))] (sphere (* s (+ 2.5 (/ d0 2))))))))
                cut1 cut2 cut3
                cut4 cut5 cut6))
              #_(translate [0 -500 0] (cube 1000 1000 1000)))))))

(spit "things/ball-socket.scad" (write-scad ball-socket))

;;; 8pin female connector cover

(def conn-cover
  (let [p 2.54 t 2 H 12 gt 2 gb 1 tol 0.3]
    (letfn [(cf [x y]
              (let [li (+ (* 8 p) tol)
                    wi (+ p tol)
                    l (+ t (- li tol) t)
                    w (+ t (- wi tol) t y t)
                    h (+ H gt gb x)
                    t (- t (/ tol 2))]
                (translate [(/ l 2) 0 0]
                           (union
                            (difference (cube l w h)
                                        (translate [0 (- (/ w 2) (/ wi 2) t)] (cube li wi (* 2 h)))
                                        (translate [0 t (- h x)] (cube (* 2 l) w h)))
                            (translate [0 0 (- (/ h 2) x (/ gb 2))]
                                       (difference (cube l w gb)
                                                   (translate [0 (- (/ w 2) (/ wi 2) t) 0] (cube l 1 (* 2 gb)))))))))
            (cf2 []
              (let [li (+ (* 6 p) tol)
                    wi (+ (* 1 p) tol)
                    li2 (+ (* 2 p) tol)
                    wi2 (+ (* 2 p) tol)
                    l (+ t (- li tol) t)
                    w (+ t (- wi2 tol) t)
                    h (+ H gt gb)
                    t (- t (/ tol 2))]
                (translate [(/ l 2) 0 0]
                           (difference (cube l w h)
                                       (translate [0 (- (/ w 2) (/ wi 2) t) (- gb)] (cube li wi h))
                                       (translate [(- (/ l 2) (/ li2 2) t) 0 (- gb)] (cube li2 wi2 h))
                                       (translate [0 (- (/ w 2) (/ wi 2) t) 0] (cube li 1 (* 2 h)))
                                       (translate [(- (/ l 2) (/ li2 2) t) (- (- (/ w 2) (/ wi 2) t)) 0]
                                                  (cube li2 1 (* 2 h)))))))]
      (union (translate [0 0 0] (cf 5 0))
             (translate [0 20 0] (cf 5 -2))
             (translate [0 40 0] (cf2))
             (translate [0 60 0] (cf2))))))

(spit "things/conn-cover.scad" (write-scad conn-cover))

;;; screw-3mm-nuts

(def screw-3mm-nuts
  (scale [0.1 0.1 0.1]
         (let [a (difference (cylinder 50 50) (cylinder 20 60))
               b (difference (cylinder 50 50) (cylinder 25 60))
               c (difference (cylinder 22 50) (cylinder 15 60))]
           (union (for [i (range 5) j (range 3)]
                    (translate [(* i 120) (* j 120) 0] a))
                  (for [i (range 5)]
                    (translate [(* i 120) -120 0] b))
                  (for [i (range 5)]
                    (translate [(* i 120) -200 0] c))))))

(spit "things/screw-3mm-nuts.scad"
      (write-scad (union screw-3mm-nuts
                         (translate [24 4 -2.4] (cube 70 60 0.2)))))

;;; primary board mounts

(def primary-board-mount-1
  (difference (translate [0 12.5 2.5] (cube 58 25 5))
              (translate [0 6 0] (cube 50 12.1 100))
              (translate [0 18.5 0] (cube 54.2 9.2 100))))

(def screw-2mm-nut-1 (translate [0 0 6.25]
                                (scale [0.1 0.1 0.1]
                                       (difference (cylinder 30 125) (cylinder 18 200)))))

(def screw-2mm-nut-2 (translate [0 0 2]
                                (scale [0.1 0.1 0.1]
                                       (difference (cylinder 30 40) (cylinder 18 200)))))

(spit "things/primary-board-mounts.scad"
      (write-scad
       (union primary-board-mount-1
              (translate [5 0 0] screw-2mm-nut-1)
              (translate [15 0 0] screw-2mm-nut-2)
              (translate [-5 0 0] screw-2mm-nut-1)
              (translate [-15 0 0] screw-2mm-nut-2))))

;;; LCD & ball mount

(def fit-block
  (let [c (rotate (/ π 2) [1 0 0] (scale [0.2 0.2 1] (cylinder 5 8)))]
    (hull
     (cube 2 8 7)
     (translate [1 0 0] c)
     (translate [-1 0 0] c))))

(def mount-lid
  (let [s (translate [0 0 2] (difference (sphere 2.2) (translate [0 0 4] (cube 8 8 8))))]
    (difference
     (union (hull
             (translate [21 23 0] s)
             (translate [21 -23 0] s)
             (translate [-21 -23 0] s)
             (translate [-21 23 0] s))
            (translate [0 0 4] (cube 36 36 8)))
     (translate [-17 14.1 5.5] fit-block)
     (translate [-17 -14.1 5.5] fit-block)
     (translate [17 -14.1 5.5] fit-block)
     (translate [17 14.1 5.5] fit-block))))

(def mount-hook
  (translate [0 0 1]
             (rotate (/ π 2) [1 0 0]
                     (difference
                      (union (translate [-0.1 0 3.5] fit-block)
                             (translate [-1 0 6] (cube 4 8 2))
                             (translate [-1 0 10] (cube 4 8 2))
                             (translate [-2 0 8] (cube 2 8 6)))
                      (translate [-2 0 2.5] (cube 2 10 5))
                      (translate [0 -4 0] (cube 100 6 100))))))

(def mount-lid-lcd
  (difference  mount-lid
               (translate [0 0 0] (cube 28 28 20))
               (translate [0 0 8] (cube 32 50 2))))

(spit "things/lcd-mounts.scad"
      (write-scad (difference (union mount-lid-lcd
                                     (translate [4 12 0] mount-hook)
                                     (translate [4 -1 0] mount-hook)
                                     (translate [-4 -1 0] mount-hook)
                                     (translate [-4 12 0] mount-hook))
                              (translate [0 0 -5] (cube 50 50 10)))))

(def mount-hook-plate
  (->>
   (union
    (difference (cube 42 42 4)
                (cube 25 50 5)
                (translate [-18 15.5 2] (cube 4 5 4))
                (translate [18 15.5 2] (cube 4 5 4))
                (translate [18 -15.5 2] (cube 4 5 4))
                (translate [-18 -15.5 2] (cube 4 5 4)))
    (cube 29 10 4)
    (translate [-13.5 0 3] (cube 2 22 2))
    (translate [13.5 0 3] (cube 2 22 2)))
   (translate [0 0 2])))

(def mount-hook-rod
  (->>
   (difference
    (union fit-block
           (translate [-1 0 13] (cube 4 8 33)))
    (translate [-3 0 0] (cube 4 16 16))
    (translate [0 -4 0] (cube 100 6 100)))
   (rotate (/ π 2) [1 0 0])
   (translate [0 0 1])))

(def mount-lid-ball
  (let [theta (* 20 (/ π 180))
        z (- 11.5 (* 20 (Math/sin theta)))
        c1 (translate [0 0 11.5] (cube 3 43 7))
        c2 (rotate (/ π 3) [0 0 1] c1)
        c3 (rotate (/ π 3) [0 0 1] c2)
        c4 (translate [12 0 6] (cube 2 50 8))
        c5 (translate [-24 0 0] c4)
        c6 (translate [0 0 (+ 5 z)] (scale [0.1 0.1 1] (cylinder 170 10)))
        c7 (translate [18 0 9] (cube 4.5 50 2.01))
        c8 (translate [-36 0 0] c7)]
    (difference (union mount-lid
                       (translate [0 0 8.5] (cube 36 36 1)))
                (translate [0 0 z] (scale [0.1 0.1 0.1] (sphere 180)))
                c1 c2 c3 c4 c5 c6 c7 c8)))

(spit "things/ball-mounts.scad"
      (write-scad (union mount-hook-plate
                         (translate [-30 40 0] mount-hook-rod)
                         (translate [-40 40 0] mount-hook-rod)
                         (translate [40 40 0] mount-hook-rod)
                         (translate [30 40 0] mount-hook-rod)
                         (translate [0 50 0] mount-lid-ball))))

(spit "things/test-ball-mount-plate.scad"
      (let [theta (* 20 (/ π 180))]
        (write-scad
         (difference
          (union ball-socket
                 (translate [0 0 11.5] (rotate [π 0 (/ π 2)] nil mount-lid-ball))
                 (translate [0 0 (* 20 (Math/sin theta))]
                            (scale [0.1 0.1 0.1] (sphere 170)))
                 (translate [(* 20 (Math/cos theta)) 0 0]
                            (rotate (/ π 2) [1 0 0] (scale [0.1 0.1 1] (cylinder 30 2.5)))))
          (translate [0 -50 0] (cube 100 100 100))))))

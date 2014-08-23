(ns vegetable.core
    (:require-macros [cljs.core.async.macros :refer [go alt!]])
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [cljs.core.async :refer [<! chan put! sliding-buffer close!]]
              [queue.core :refer [qcreate qpush qpop]]
              clojure.string
              clojure.set
              
    ))


(enable-console-print!)


(defn yielding_dfs [init neighs good?]
  (let [_init (init)
        construct_path
            (fn recurse [path visited end_vertex]
                (conj! path end_vertex)
                (if (= end_vertex _init)
                    (persistent! path)
                    (recurse path visited (get visited end_vertex))
                )   
            )
        make_step
            (fn recurse [visited queue]
              (let [current (qpop queue)
                    neighbours (neighs current)]
                   (doseq [n neighbours] (if (= (get visited n) nil) (do (assoc! visited n current) (qpush queue n)) nil))
                   (if (good? current) {:final true :result (construct_path (transient []) visited current)} 
                                       {:final false :run (fn [] (recurse visited queue))})
              )                        
            )
        queue (qcreate 100000)
        ]    
         (qpush queue _init)
         (make_step (transient (hash-map _init _init)) queue)
  )      
) 

(defn not_yielding_dfs [init neighs good?]
  (def res {:final false :run #(yielding_dfs init neighs good?)})
  (while (not (:final res))
    (def res ((:run res)))
  )
  res
)


(defn dfs [init neighs good?]
  (let [queue (qcreate 1000000)
        _init (init)
        visited (transient (hash-map _init _init))]
          (qpush queue _init)
          (def should_end false)
          (while (not should_end)
            (let [current (qpop queue)
                  neighbours (neighs current)]
                 (if (good? current) (do (def good_vertex current) (def should_end true)))
                 (doseq [n neighbours] (if (= (get visited n) nil) (do (assoc! visited n current) (qpush queue n)) nil))))
          (def current good_vertex)
          (def path (transient [good_vertex]))
          (while (and (not= current nil) (not= current _init))
            (def current (get visited current))
            (conj! path current))
          {:final true :result (persistent! path)}))

(def dimension 8)

(defn get_random_data [] 
  (into [] (for [y (range dimension)] (into [] (for [x (range dimension)] (if (< (rand) 0.97) :clear (if (< (rand) 0.5) :yes :no))))))
)

;(def state (atom {:array (get_random_data) :path [[0 0] [1 0] [2 0] [2 1] [2 2] [1 2] [1 1] [0 1] [0 0]]}))
(def state (atom {:array (get_random_data) :path nil}))

(def next_option {:clear :yes :yes :no :no :clear})

(defn click [v x y]
    (let [array (v :array)
          col (array x)
          cell (col y)
         ] 
         {:array (assoc array x (assoc col y (next_option cell))) :path nil}
    )
)

(defn sum [vct] (reduce + vct))

(defn cmp_path [array]

    (defn indexof [elem vect]
      (if (= elem (first vect))
          0
          (+ 1 (indexof elem (rest vect)))))

    (def points 
      (into [] (filter (fn [rec] (not (nil? (rec 1)))) (for [i (range dimension) j (range dimension)] [[i j] 
          (case ((array i) j) 
             :clear nil
             :yes :yes
             :no :no
          )]
      )))
    )

    (def bare_points (into [] (for [[point v] points] point)))

    (def xors (into {} (for [i (range dimension) j (range dimension)] [[i j] (sum (for [k (range j dimension) :when (not (= ((array i) k) :clear))] 
        (bit-shift-left 1 (indexof [i k] bare_points))))])))

    (def good_lines
      (sum (for [[index [point yesno]] (map-indexed vector points) :when (= yesno :yes)] (bit-shift-left 1 index))))

    (defn good [state]
        (= state {:x 0 :y 0 :lines good_lines}))

    (defn get_init_state [] 
        {:x 0 :y 0 :lines 0 })
    
    (defn neighs [state]
        (let [x (state :x)
              y (state :y)
              lines (state :lines)
             ]
             (filter #(not (nil? %)) (for [[dx dy] [[0 1] [1 0] [-1 0] [0 -1]]]
                   (let [nx (+ x dx)
                         ny (+ y dy)
                        ]
                        (if (not (and (>= nx 0) (>= ny 0) (<= nx dimension) (<= ny dimension))) nil
                           (let [mx (min x nx)
                                 nlines (if (= 0 dx) lines (bit-xor lines (xors [mx y])))
                                 ]
                                 {:x nx :y ny :lines nlines}
                           )
                        )
                                    
                   )
             ))
        )
    )
    {:init get_init_state :neighs neighs :good good}
) 


(def events (chan 10))

(defn get_edges [fir sec rst horiz vert]
  (let
    [edge [[(min (fir 0) (sec 0)) (min (fir 1) (sec 1))]
           [(max (fir 0) (sec 0)) (max (fir 1) (sec 1))]]
     horiz (if (= (fir 0) (sec 0)) horiz (cons edge horiz))
     vert (if (= (fir 1) (sec 1)) vert (cons edge vert))
    ]
    (if (= [] rst) [(into {} horiz) (into {} vert)] (get_edges sec (first rst) (rest rst) horiz vert))
  )
)

(defn widget [data owner]
    (reify
        om/IRender
        (render [this]
            (let [path (@state :path)
                  [horiz_edges vert_edges] (if path (get_edges (first path) (first (rest path)) (rest (rest path)) [] []) [[] []])
                 ]
                 (apply dom/div (cons nil 
                      (for [y (range dimension)] 
                          (apply dom/div (cons #js {:className "row" } 
                            (for [x (range dimension)] 

                              (let [vert_classname (if (contains? vert_edges [x y]) "left_line" "")
                                    horiz_classname (if (contains? horiz_edges [x y]) "top_line" "")
                                    fill_classname (name (((@state :array) x) y))
                                   ]

                                (dom/div #js {:className (clojure.string/join " " 
                                                ["cell" vert_classname horiz_classname fill_classname]) 
                                               :onClick #(put! events [x y owner])
                                              } "") 
                              )
                            )
                          ))
                      )
                 ))
            )
         )
       om/IInitState
         (init-state [_] 
         {:text "Hello world!"})


       om/IWillMount
         (will-mount [_] (put! events [0 0 owner]))
    )
)


(om/root widget {}
           {:target (. js/document (getElementById "my-app"))})

(defn compute_path []
  (def ticker (chan (sliding-buffer 1)))
  (go
     (println "inside compute go")
     (let [ing (cmp_path (:array @state))]
       (def orig_array (:array @state))
       (.setTimeout js/window #(put! ticker :tick) 1)
       (loop [res {:final false :run #(yielding_dfs (ing :init) (ing :neighs) (ing :good))}] 
         (if (res :final) 
           (into [] (for [x (res :result)] [(x :x) (x :y)]))
           (do
             (<! ticker)
             (.setTimeout js/window #(put! ticker :tick) 1)
             (recur ((res :run)))
           )
         )
       )
     )
  )
)


(go 
  (loop [last_computation (chan)]
    (let [[x y owner] (<! events)]
         (println "event cought" x y owner)
         (close! last_computation)
         (swap! state click x y) 
         (om/refresh! owner)
         (recur 
           (go
             (let [path (<! (compute_path))]
                    (swap! state (fn [vl] (assoc vl :path path)))
                    (om/refresh! owner)
             )
           )
         )
    )
  )
)


;(println "dakujeme vam 4 krat!")


;{{{ ticker example
;(defn example2 [] 
;  (def ticker (chan (sliding-buffer 1)))
;  (def ticker2 (chan (sliding-buffer 1)))
;
;  (go
;    (while true
;      (let [v (<! ticker)]
;        (println "ticker" v)
;        (>! ticker 0)
;        (.setTimeout js/window #(put! ticker (+ 1 v)) 0)
;      )
;    )
;  )
;
;  (go
;    (while true
;      (let [v (<! ticker2)]
;        (println "---- ticker2" v)
;        (.setTimeout js/window #(put! ticker2 (+ 1 v)) 0)
;      )
;    )
;  )
;
;  (go
;    (>! ticker 0)
;    (>! ticker2 0)
;  )
;)

;(example2)
;}}}



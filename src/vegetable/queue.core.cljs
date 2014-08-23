(ns queue.core
)

(defn qempty? [queue]
  (= (:begin queue) (:end queue))
)

(defn nxt [queue index]
  (mod (+ 1 index) (count (get queue :list)))
)

(defn qcreate [capacity]
  (transient {:begin 0 :end 0 :list (transient (into [] (replicate capacity nil)))}) 
)

(defn qpush [queue elem]
  (assoc! (:list queue) (:end queue) elem)
  (assoc! queue :end (nxt queue (:end queue)))
  (if (qempty? queue)
    (throw (js/Error. "Not enough space in the queue"))
  )
  queue
)

(defn qpop [queue]
  (if (qempty? queue)
    (throw (js/Error. "Cannot pop from empty queue"))
  )
  (def res ((:list queue) (:begin queue)))
  (assoc! queue :begin (nxt queue (:begin queue)))
  res
)


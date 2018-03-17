(ns split-bill.split
  (:require [cljs.pprint :as pprint]))

(defn map-vals
  "Return map `m` with function `f` applied to the values.

  See discussion: https://dev.clojure.org/jira/browse/CLJ-1959 and
  https://www.reddit.com/r/Clojure/comments/7coeu6/."
  [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty m) m))

(defn update-vals
  "Update the values of multiple keys `ks` with function `f`.

  See discussion: https://stackoverflow.com/questions/9638271/ and
  http://dacamo76.com/blog/2014/11/28/updating-map-values-in-clojure/."
  [m ks f]
  (reduce (fn [m k] (update-in m [k] f)) m ks))

(defn round
  "Round `number` to 2 decimal places.

  FIXME returns `##NaN`. Do not use until fixed."
  [number]
  (js/parseFloat (pprint/cl-format nil "%.2f" number)))

(defn add-percentage
  "Add `percentage` to `total`."
  [total percentage]
  (+ (* percentage total) total))

(defn total
  "Return the total for a given `subtotal`, `tax`, and `tip`."
  [subtotal tax tip]
  (-> subtotal (add-percentage tax) (add-percentage tip)))

(defn cut
  "Return the portion of the total owed given `orders`."
  [orders subtotal tax tip]
  (* (/ (apply + orders) subtotal)
     (total subtotal tax tip)))

(defn split
  "Divvy the `price` among `splitees` for a `party`."
  [party price splitees]
  (let [portion (/ price (count splitees))]
    (update-vals party splitees #(conj % portion))))

(defn bill
  "Return a map with the calculated the total and what each person owes, `:total`
  and `:owed` respectively."
  [{:keys [party subtotal tax tip] :as m}]
  {:split party
   :owed (map-vals party #(cut % subtotal tax tip))
   :total (total subtotal tax tip)})

(defn receipt
  "Pretty print the bill."
  [m]
  (pprint/pprint (bill m)))

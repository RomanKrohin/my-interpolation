(ns interpolation.core
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-s" "--step STEP" "Шаг интерполяции" :parse-fn #(Double/parseDouble %)]
   ["-a" "--algorithm ALGORITHM" "Алгоритм интерполяции (linear, lagrange, both)"
    :validate [#{"linear" "lagrange" "both"} "Должен быть 'linear', 'lagrange' или 'both'"]]
   ["-h" "--help"]])

(defn parse-line [line]
  (let [[x y] (str/split line #"[;\t\s]+")]
    [(Double/parseDouble x) (Double/parseDouble y)]))

(defn linear-interpolation [points step]
  (let [[p1 p2] points
        [x1 y1] p1
        [x2 y2] p2
        x-values (range x1 (+ step x2) step)]
    (map (fn [x]
           (let [t (/ (- x x1) (- x2 x1))]
             [(double x) (+ y1 (* t (- y2 y1)))]))
         x-values)))

(defn lagrange-polynomial [points x]
  (let [n (count points)]
    (reduce
     (fn [acc i]
       (let [[xi yi] (nth points i)
             li (reduce (fn [li j]
                          (if (not= i j)
                            (let [[xj _] (nth points j)]
                              (* li (/ (- x xj) (- xi xj))))
                            li))
                        1
                        (range n))]
         (+ acc (* li yi))))
     0
     (range n))))

(defn lagrange-interpolation [points step start-x end-x]
  (let [x-values (range start-x (+ end-x step) step)]
    (map (fn [x] [x (lagrange-polynomial points x)]) x-values)))

(defn format-output [x-values y-values]
  (let [x-str (str/join "\t" (map #(format "%.2f" %) x-values))
        y-str (str/join "\t" (map #(format "%.2f" %) y-values))]
    (str x-str "\n" y-str "\n")))

(defn process-input [step algorithm]
  (let [window-size 5]
    (loop [points []]
      (let [line (read-line)]
        (if (nil? line)
          (do
            (println "EOF получен. Завершение работы.")
            (System/exit 0))
          #_{:clj-kondo/ignore [:not-empty?]}
          (if (not (empty? line))
            (let [point (parse-line line)
                  updated-points (conj points point)
                  sorted-points (if (not (apply <= (map first updated-points)))
                                  (do
                                    (println "Данные не отсортированы по X. Производится сортировка.")
                                    (sort-by first updated-points))
                                  updated-points)]

              (when (>= (count sorted-points) 2)
                (when (or (= algorithm "linear") (= algorithm "both"))
                  (let [window-points (take-last 2 sorted-points)
                        interp (linear-interpolation window-points step)
                        x-values (map first interp)
                        y-values (map second interp)]
                    (println (format "Линейная интерполяция (X от %.3f до %.3f):"
                                     (first x-values) (last x-values)))
                    (println (format-output x-values y-values)))))

              (let [lagrange-windows (partition window-size 1 sorted-points)]
                (doseq [window lagrange-windows]
                  (let [start-x (first (map first window))
                        end-x (last (map first window))
                        interp (lagrange-interpolation window step start-x end-x)
                        x-values (map first interp)
                        y-values (map second interp)]
                    (println (format "Лагранжевская интерполяция (X от %.3f до %.3f):"
                                     start-x end-x))
                    (println (format-output x-values y-values)))))

              (recur sorted-points))
            (recur points)))))))

(defn -main [& args]
  (let [{:keys [options]} (parse-opts args cli-options)]
    (if (:help options)
      (println "Usage: lein run --step <step> --algorithm <algorithm>")
      (let [step (or (:step options) 1.0)
            algorithm (or (:algorithm options) "both")]
        (println "Введите точки в формате: X Y")
        (process-input step algorithm)))))

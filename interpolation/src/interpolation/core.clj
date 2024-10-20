(ns interpolation.core
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-s" "--step STEP" "Шаг интерполяции" :parse-fn #(Double/parseDouble %)]
   ["-h" "--help"]])

(defn parse-line [line]
  (let [[x y] (str/split line #"\s+")]
    [(Double/parseDouble x) (Double/parseDouble y)]))

(defn linear-interpolation [points step]
  (let [[p1 p2] points
        [x1 y1] p1
        [x2 y2] p2
        x-values (range x1 (+ x2 step) step)]
    (map (fn [x]
           (let [t (/ (- x x1) (- x2 x1))]
             [x (+ y1 (* t (- y2 y1)))]))
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
    (str x-str "\n" y-str)))

(defn process-input [step]
  (let [window (atom [])]
    (while true
      (let [line (read-line)]
        (when (not (empty? line))
          (let [point (parse-line line)]
            (swap! window conj point)

            ;; Линейная интерполяция: выполняется после каждых 2 точек
            (when (>= (count @window) 2)
              (let [window-points (take-last 2 @window)
                    interp (linear-interpolation window-points step)
                    x-values (map first interp)
                    y-values (map second interp)]
                (println (format "Линейная интерполяция (X от %.3f до %.3f):"
                                 (first x-values) (last x-values)))
                (println (format-output x-values y-values))))

            ;; Лагранжевая интерполяция: выполняется после каждых 5 точек
            (when (>= (count @window) 5)
              (let [window-points (take-last 5 @window)
                    start-x (first (map first window-points))
                    end-x (last (map first window-points))
                    interp (lagrange-interpolation window-points step start-x end-x)
                    x-values (map first interp)
                    y-values (map second interp)]
                (println (format "Лагранжевская интерполяция (X от %.3f до %.3f):"
                                 start-x end-x))
                (println (format-output x-values y-values))))))))))

(defn -main [& args]
  (let [{:keys [options]} (parse-opts args cli-options)]
    (if (:help options)
      (println "Usage: lein run --step <step>")
      (let [step (or (:step options) 1.0)]
        (println "Введите точки в формате: X Y")
        (process-input step)))))

(ns interpolation.core
  (:require [clojure.string :as str])
  (:gen-class))

;; 1. Функция для парсинга входной строки
(defn parse-line [line]
  (let [[x y] (str/split line #"\s+")]
    [(Double/parseDouble x) (Double/parseDouble y)]))

;; 2. Линейная интерполяция между двумя точками
(defn linear-interpolation [p1 p2 step]
  (let [[x1 y1] p1
        [x2 y2] p2
        x-values (range x1 (+ x2 step) step)]
    (map (fn [x]
           (let [t (/ (- x x1) (- x2 x1))]
             [x (+ y1 (* t (- y2 y1)))]))
         x-values)))

;; 3. Интерполяция Лагранжа для множества точек
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

;; 4. Вычисление Лагранжевой интерполяции на множестве точек
(defn lagrange-interpolation [points step start-x end-x]
  (let [x-values (range start-x (+ end-x step) step)]
    ;; Проверяем корректность диапазона
    (if (empty? x-values)
      (throw (IllegalArgumentException. "Диапазон x-значений пуст! Убедитесь, что start-x и end-x различаются."))
      (map (fn [x] [x (lagrange-polynomial points x)]) x-values))))

;; 5. Чтение и обработка входных данных
(defn process-input []
  (let [data (atom [])]
    (while true
      (let [line (read-line)]
        (when (not (empty? line))
          (let [point (parse-line line)]
            (swap! data conj point)
            (println "Введены точки: " @data)

            ;; Линейная интерполяция при наличии 2 точек
            (when (>= (count @data) 2)
              (let [[p1 p2] (take-last 2 @data)]
                (println "Линейная интерполяция:")
                (doseq [[x y] (linear-interpolation p1 p2 1)]
                  (println (format "%.2f\t%.2f" x y)))))

            ;; Лагранжевая интерполяция при наличии минимум 3 точек
            (when (>= (count @data) 3)
              (println "Лагранжевая интерполяция:")
              (try
                (doseq [[x y] (lagrange-interpolation @data 1 (ffirst @data) (ffirst (last @data)))]
                  (println (format "%.2f\t%.2f" x y)))
                (catch IllegalArgumentException e
                  (println (.getMessage e)))))))))))

;; 6. Главная функция программы
(defn -main [& args]
  (println "Введите точки в формате: X Y")
  (process-input))

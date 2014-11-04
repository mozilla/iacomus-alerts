(ns iacomus-alerts.core
  (:require [clj-time.core :as time]
            [clj-time.format :as timef]
            [clj-time.periodic :as timep]
            [cheshire.core :as json]
            [clojure-csv.core :as csv]
            [clojure.string :as string]
            [incanter.stats :as stats]
            [incanter.core :refer [abs]]))

(def time-formatter (timef/formatter "yyyyMMdd"))
(def time-medusa-formatter (timef/formatter "yyyy-MM-dd"))
(def iacomus-formatter (timef/formatter "EEE MMM dd YYYY"))

(def load-config
  (memoize
   (fn [url]
     (println "Fetching configuration: " url)
     (with-open [in (clojure.java.io/input-stream url)]
       (let [config (-> (slurp in) (json/parse-string true))]
         (assoc config :url url))))))

(def load-report
  (memoize
   (fn [url]
     (println "Fetching report: " url)
     (try
       (with-open [in (java.util.zip.GZIPInputStream.
                       (clojure.java.io/input-stream url))]
         (slurp in))
       (catch java.io.IOException e
         nil)))))

(defn report [{:keys [url-prefix] :as config} day]
  ;; Returns the most recent available report starting from day
  (loop [day day, retrials 0]
    (let [url (str url-prefix "_" (timef/unparse time-formatter day) ".csv.gz")]
      (if-let [report (load-report url)]
        [day report]
        (when (< retrials 10)
          (recur (time/minus day (time/days 1)) (+ 1 retrials)))))))

(defn reports [{:keys [url-prefix] :as config} day]
  ;; Returns a lazy sequence of the reports for a dashboard in reverse cronological
  ;; order starting from day
  (let [[day report] (report config day)]
    (cons [day (rest (csv/parse-csv report))]
          (when report
            (lazy-seq (reports config (time/minus day (time/days 1))))))))

(defn sort-attributes [{{values :values} :sort-options, header :header}]
  ;; Returns an attribute accessor function for each sorting option
  (let [header-index (zipmap header (iterate inc 0))]
    (for [value values]
      [value #(get % (header-index value))])))

(defn primary-key [{:keys [primary-key header]}]
  ;; Returns the primary key of the configuration expressed as a function
  (let [header-index (zipmap header (iterate inc 0))]
    #(map (partial get %) (map header-index primary-key))))

(defn index-by-primary-keys [primary-key [date entries :as report]]
  ;; Returns a report that contains a map that associates the values of the
  ;; primary keys to the entries
  [date (zipmap (map primary-key entries) entries)])

(defn coerce [value]
  (->> value
       (re-find #"\d+.\d+|\d+")
       (Double/parseDouble)))

(defn top-metrics [primary-key [date entries] attribute]
  ;; Returns a sequence of all possible values that the primary key takes
  ;; in the 10 "hottest" entries of a report
  (let [cnv (fn [entry]
              (->> entry
                   attribute
                   coerce))
        entries (take 10 (sort-by cnv > entries))]
    (map primary-key entries)))

(defn timeseries [reports metric attr]
  ;; Returns a sequence of datapoints for metric where a datapoint is a tuple of
  ;; [date, attribute value]
  (map (fn [[date entries]]
         (let [row (get entries metric)
               value (attr row)]
           [date (if value
                   (coerce value)
                   nil)]))
       reports))

(defn outlier-sd? [sample value]
  ;; Assuming the sample comes from a Normal distribution, can value be considered an outlier?
  (let [mean (stats/mean sample)
        sd (stats/sd sample)
        threshold (* 6 sd)] ;; 6 Sigma seems to do the trick, outliers are the norm here
    (when (or (> value (+ mean threshold))
              (< value (- mean threshold)))
      [mean sd threshold])))

(defn is-outlier? [timeseries [date point]]
  ;; Returns true if the datapoint can be considered an outlier in the timeseries
  (when (and timeseries date point)
    (let [count-points (count timeseries)
          timeseries (->> timeseries
                          (map second)
                          (filter identity))]
      (when (> (count timeseries) (/ count-points 2)) ;; Ignore timeseries with many NAs
        (when-let [[mean sd threshold] (outlier-sd? timeseries point)]
          {:mean mean, :sd sd, :threshold threshold})))))

(defn box-plot [attr xs value date]
  (let [xs (filter identity xs)
        lq (stats/quantile xs :probs 0.25)
        hq (stats/quantile xs :probs 0.75)
        iqr (- hq lq)
        lf (apply min xs)
        hf (apply max xs)]
    [attr lf lq hq hf value date]))

(defn generate-alert [{:keys [url]} attr-name {:keys [date value series metric mean sd threshold]}]
  (let [metric (string/join ":" (concat [attr-name] metric))
        iacomus-date (timef/unparse iacomus-formatter (time/plus date (time/weeks 2)))
        date (timef/unparse time-medusa-formatter date)
        alert {:date date
               :boxplot (box-plot attr-name series value date)
               :title metric
               :link (str "http://mozilla.github.io/iacomus/resources/public/index.html#?config=" url
                          "&sort=" attr-name
                          "&base-date=" iacomus-date)
               :type "boxplot"}]
    alert))

(defn alerts [config time]
  ;; Runs the detection system for the specified iacomus configuration file
  ;; After seeing some of the (probably) misclassified alerts I start thinking
  ;; that using bayesian inference on windows would be better suited for this problem...
  (let [pk (primary-key config)
        attrs (sort-attributes config)
        reports-source (reports config time)
        latest-report (first reports-source)
        reports (->> reports-source
                     (take 9)
                     (map (partial index-by-primary-keys pk)))]
    (->> (for [[attr-name attr] attrs]
           (let [metrics (top-metrics pk latest-report attr)]
             (for [metric metrics]
               (let [[f latest & series :as ts] (timeseries reports metric attr)
                     statistic (is-outlier? series latest)
                     statistic-f (is-outlier? series f)] ;; Reduce the probability of dealing with a statistical fluctuation...
                 (when (and statistic statistic-f)
                   (generate-alert config
                                   attr-name
                                   (merge statistic
                                          {:metric metric
                                           :series (map second series)
                                           :value (second latest)
                                           :date (first latest)})))))))
         (apply concat)
         (filter identity))))

(defn dump-alerts [alerts]
  (let [output (json/generate-string alerts)]
    (spit "alerts.json" output)))

(defn detect
  ([configs]
     (detect [(time/now)]))
  ([configs days]
     (->> (for [day days]
            (for [{[title description] :title :as config} (map load-config configs)]
              (let [a (alerts config day)]
                {:title title
                 :description description
                 :alerts a})))
          (apply concat)
          (dump-alerts))))



(defn -main []
  (detect ["https://s3-us-west-2.amazonaws.com/telemetry-public-analysis/mainthreadio/data/iacomus.json"]
          [(time/minus (time/now) (time/weeks 1))]))

;; (detect ["https://s3-us-west-2.amazonaws.com/telemetry-public-analysis/mainthreadio/data/iacomus.json"]
;;         (take 32 (timep/periodic-seq (time/minus (time/now) (time/days 35)) (time/days 1))))

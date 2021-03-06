(ns grad.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as string]
            [hiccup.core :as hiccup])
  (:import (java.util UUID))
  (:gen-class))

(def sep "/")
(def temp-dir "grad")

;; utils
;; -------------

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))


(defn str->int [s]
  (Integer. s))

(defn remove-accent [s]
  (. (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
     replaceAll "\\p{M}", ""))


(defn empty-line? [s]
  (= (count (string/trim s)) 0))

(defn avg [coll]
  (round2 2
          (float
            (/ (apply + coll)
               (count coll)))))

(def not-empty-line? (partial (comp not empty-line?)))

(defn uuid-v3 [^String name]
  (str (UUID/nameUUIDFromBytes (.getBytes name))))

(defn is-c? [^java.io.File file]
  (clojure.string/ends-with? file ".c"))

(defn is-cpp? [^java.io.File file]
  (clojure.string/ends-with? file ".cpp"))

(defn file-name [^java.io.File file]
  (last (clojure.string/split (str file) #"/")))

;; TODO create temp dir
(defn temp-file-name [^java.io.File file]
  (let [name (file-name file)
        ext (second (clojure.string/split name #"\."))]
    (java.io.File/createTempFile (uuid-v3 name) (str "." ext) #_(java.io.File. temp-dir))))

(defn copy-to-temp [source-file]
  (let [dest-file (temp-file-name source-file)]
    (io/copy (io/file source-file) (io/file dest-file))
    dest-file))

(defn compiles? [file]
  (let [compiler (if (is-cpp? file) "g++" "cc")]
    (let [exec (sh/sh compiler (str file))]
      #_(println exec)
      (-> exec
          :exit
          (= 0)))))

(defn format-file [^java.io.File file]
  (sh/sh "vim" "-c" "argdo setf cpp | execute 'normal! gg=G'| update " "-c" "quitall" (str file)))

#_(format-file "/tmp/a105e6bf-7167-3eca-86cd-3af4e29bf7013768352428308865292.cpp")

(defn diff-files [source temp]
  (let [cmd (clojure.string/join " " ["diff" (str "\"" source "\"") (str temp) "|" "grep" "-P" "'^[>]'" "|" "wc" "-l"])]
    (-> (sh/sh "bash" "-c" cmd)
        :out
        (clojure.string/split #"\n")
        first
        str->int
        )))

(defn student-name [source]
  (let [chunks (clojure.string/split (str source) #"/")
        len (count chunks)]
    (if-let [folder (get chunks (- len 2))]
      (let [chunks' (clojure.string/split folder #"_")]
        (str (nth chunks' 2) " " (nth chunks' 3))))))

(defn line-cnt [source]
  (let [lines (string/split (slurp source) #"\r?\n")]
    (count (filter not-empty-line? lines))))

(defn avg-line-len [source]
  (let [lines (string/split (slurp source) #"\r?\n")]
    (avg (map count (filter not-empty-line? lines)))))

#_(avg-line-len file)

(defn process-file [file]
  (let [temp-file (copy-to-temp file)
        compiles?? (compiles? temp-file)
        _ (format-file temp-file)
        format-diff (diff-files file temp-file)
        student-name (student-name file)
        line-cnt' (line-cnt file)
        avg-line-len' (avg-line-len file)]
    {:file-name (file-name file)
     :source-file (str "file://" file)
     :compiles? compiles??
     :format-diff format-diff
     :student-name (-> student-name remove-accent string/upper-case)
     :line-cnt line-cnt'
     :avg-line-len avg-line-len'}))

(defn process-dir [work-dir]
  (let [data (->> (file-seq (io/file work-dir))
                  (filter #(or (is-c? %) (is-cpp? %)))
                  vec)]
    (pmap process-file data)))

(defn submission-comp [f1 f2]
  (compare [(:student-name f1) (:file-name f1)]
           [(:student-name f2) (:file-name f2)]))

(comment
  (def data (process-dir work-dir)))

(comment
  data)

(comment
  (clojure.pprint/print-table [:student-name :file-name :compiles? :format-diff :line-cnt :avg-line-len :source-file] (sort submission-comp data)))

(defn process-and-print [source-dir out-file]
  (let [data (process-dir source-dir)]
    (spit out-file (hiccup/html
                       [:html
                        [:head
                         [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                         [:link {:rel "stylesheet" :href "https://unpkg.com/purecss@2.0.3/build/pure-min.css"}]]
                        [:body
                         [:table {:class "pure-table pure-table-bordered"}
                          [:thead
                           [:tr
                            [:td :student-name]
                            [:td :file-name]
                            [:td :compiles?]
                            [:td :format-diff]
                            [:td :line-cnt]
                            [:td :avg-line-len]
                            [:td :source-file]]]
                          [:tbody
                           (for [d (sort submission-comp data)]
                             [:tr
                              [:td (d :student-name)]
                              [:td (d :file-name)]
                              [:td (d :compiles?)]
                              [:td (d :format-diff)]
                              [:td (d :line-cnt)]
                              [:td (d :avg-line-len)]
                              [:td [:a {:href (d :source-file)} "file"]]
                              ]
                             )]]]]))))

(defn -main [& args]
  (process-and-print (first args) (second args)))

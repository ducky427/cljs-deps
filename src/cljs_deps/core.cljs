(ns cljs-deps.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! chan <!] :as async]
            [clojure.set :as cset]
            [clojure.string :as string]))

(nodejs/enable-util-print!)

(def fs (nodejs/require "fs"))
(def request (nodejs/require "request"))
(def xml2js (nodejs/require "xml2js"))
(def path (nodejs/require "path"))

(def out-dir "jars")

(def default-repositories
  ["https://repo1.maven.org/maven2/"
   "https://clojars.org/repo/"])

(defn make-request
  [url]
  (let [out (chan)]
    (.get request #js {:url url}
          (fn [error response body]
            (put! out [error response body])))
    out))

(defn parse-xml
  [x]
  (let [out (chan)]
    (.parseString xml2js x (fn [err result]
                             (put! out [err result])))
    out))

(defn short-base-url [group artifact version]
  (let [group (string/replace group "." "/")]
    (str group "/"
         artifact "/"
         version "/")))

(defn base-url
  [group artifact version]
  (str (short-base-url group artifact version)
       artifact "-"
       version))

(defn base-jar-url
  [group artifact version]
  (str (base-url group artifact version) ".jar"))

(defn base-pom-url
  [group artifact version]
  (str (base-url group artifact version) ".pom"))

(defn pom-urls
  [{:keys [group artifact version]}]
  (map #(str % (base-pom-url group artifact version)) default-repositories))

(defn jar-urls
  [{:keys [group artifact version]}]
  (map #(str % (base-jar-url group artifact version)) default-repositories))

(defn jar-file-name
  [{:keys [group artifact version]}]
  (str group "-" artifact "-" version ".jar"))

(defn- get-xml-text
  [n k]
  (get-in n [k 0]))

(def ^:private deps-xform (comp
                           (remove #(= (:scope %) ["test"]))
                           #_(remove #(= (:artifactId %) ["clojure"]))
                           (map (fn [x]
                                  {:group (get-xml-text x :groupId)
                                   :artifact (get-xml-text x :artifactId)
                                   :version (get-xml-text x :version)}))))

(defn- parse-dependencies
  [[_ x]]
  (let [doc   (js->clj x :keywordize-keys true)
        deps  (get-in doc [:project :dependencies 0 :dependency])]
    (into #{} deps-xform deps)))

(defn get-project-dependencies
  [project]
  (let [out  (chan)]
    (go
      (loop [urls  (pom-urls project)]
        (if (seq urls)
          (let [url                    (first urls)
                [error response body]  (<! (make-request url))]
            (if (= 200 (.-statusCode response))
              (>! out (parse-dependencies
                       (<! (parse-xml body))))
              (recur (rest urls))))
          (async/close! out))))
    out))

(defn get-all-dependencies
  [project]
  (let [out      (chan)
        projects #{project}]
    (go
      (loop [projects   projects
             all-deps   #{}
             processed  #{}
             nf         #{}]
        (if (seq projects)
          (let [p             (first projects)
                deps          (if (contains? processed p)
                                #{}
                                (<! (get-project-dependencies p)))
                new-projects  (cset/union (rest projects) deps)
                new-processed (conj processed p)
                nnf           (if (nil? deps)
                                (conj nf p)
                                nf)]
            (if deps
              (recur new-projects
                     (cset/union all-deps deps)
                     new-processed
                     nnf)
              (recur new-projects
                     all-deps
                     new-processed
                     nnf)))
          (>! out [all-deps nf]))))
    out))

(defn download-dependency
  [dep]
  (let [out (chan)]
    (go
      (loop [urls  (jar-urls dep)]
        (if (seq urls)
          (let [url                    (first urls)
                [error response body]  (<! (make-request url))]
            #_(.. (.get request url)
                (.pipe )
                )
            (if (= 200 (.-statusCode response))
              (do
                (println "Downloaded: " dep)
                (.writeFileSync fs
                                (.join path out-dir (jar-file-name dep))
                                body
                                "binary")
                (>! out dep))
              (recur (rest urls))))
          (async/close! out))))
    out))

(defn download-dependencies
  [xs]
  (let [out (chan)]
    (go
      (loop [deps xs]
        (if (seq deps)
          (let [x (first deps)
                y (<! (download-dependency x))]
            (println x y)
            (recur (rest deps)))
          (async/close! out))
        ))
    out))


(def proj  {:group "org.clojure" :artifact "clojurescript" :version "1.8.51"})
#_(def proj {:group "org.clojure" :artifact "clojure" :version "1.8.0"})

(defn -main
  []
  (go
    (let [[deps nf]  (<! (get-all-dependencies proj))]
      (println "Deps: " deps)
      (when (seq nf)
        (println "Not found: " nf))
      (<! (download-dependencies
           (take 5
                 (conj deps proj)))))))

(set! *main-cli-fn* -main)

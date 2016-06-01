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
  {:maven  "https://repo1.maven.org/maven2/"
   :clojars "https://clojars.org/repo/"})

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
  (map (fn [[k v]]
         {:url (str v (base-pom-url group artifact version))
          :repo k})
       default-repositories))

(defn jar-url
  [{:keys [group artifact version repo]}]
  (str (repo default-repositories) (base-jar-url group artifact version)))

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
          (let [{:keys [url repo]}     (first urls)
                [error response body]  (<! (make-request url))]
            (if (= 200 (.-statusCode response))
              (>! out [repo
                       (parse-dependencies
                        (<! (parse-xml body)))])
              (recur (rest urls))))
          (async/close! out))))
    out))

(defn get-all-dependencies
  [project]
  (let [out      (chan)
        projects #{project}]
    (go
      (loop [projects   projects
             processed  #{}
             nf         #{}]
        (if (seq projects)
          (let [p             (first projects)
                [repo deps]   (if (or (contains? processed p)
                                      (contains? nf p))
                                [nil #{}]
                                (<! (get-project-dependencies p)))
                new-processed (if repo
                                (conj processed (assoc p :repo repo))
                                processed)
                nnf           (if (nil? deps)
                                (conj nf p)
                                nf)]
            (recur (cset/union (rest projects) deps)
                   new-processed
                   nnf))
          (>! out [nf processed]))))
    out))

(defn- ^boolean file-exists
  [f]
  (try
    (do
      (.statSync fs f)
      true)
    (catch js/Object e
      false)))

(defn download-dependency
  [dep]
  (let [out (chan)]
    (go
      (let [url    (jar-url dep)
            f-name (.join path out-dir (jar-file-name dep))]
        (if (file-exists f-name)
          (>! out dep)
          (.. (.get request url)
              (on "response" (fn [response]
                               #_(println (.-statusCode response))))
              (pipe (.createWriteStream fs f-name))
              (on "finish" (fn []
                             (println "Downloaded: " dep)
                             (put! out dep)))))))
    out))

(defn download-dependencies
  [xs]
  (let [out (chan)]
    (go
      (loop [deps xs]
        (if (seq deps)
          (let [x (first deps)
                y (<! (download-dependency x))]
            (when-not y
              (println "Unable to download: " x))
            (recur (rest deps)))
          (async/close! out))))
    out))


#_(def proj  {:group "org.clojure" :artifact "clojurescript" :version "1.8.51"})
#_(def proj {:group "org.clojure" :artifact "clojure" :version "1.8.0"})
(def proj {:group "reagent" :artifact "reagent" :version "0.6.0-alpha2"})

(defn -main
  []
  (go
    (let [[nf processed]  (<! (get-all-dependencies proj))]
      (when (seq nf)
        (println "Not found: " nf))
      (println "To download: " processed)
      (println "Number of jars to download: "(count processed))
      (<! (download-dependencies processed)))))

(set! *main-cli-fn* -main)

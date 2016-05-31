(defproject cljs-deps "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :min-lein-version "2.5.3"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.51"]
                 [org.clojure/core.async "0.2.374"]]

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-ancient "0.6.10"]
            [lein-figwheel "0.5.3-2"]
            [lein-cljfmt "0.5.3"]]

  :source-paths ["src"]

  :clean-targets ["main.js"
                  "target"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :figwheel true
              :compiler {
                :main cljs-deps.core
                :output-to "main.js"
                :target :nodejs
                :output-dir "out"
                :optimizations :none
                :parallel-build true
                :source-map true}}]})
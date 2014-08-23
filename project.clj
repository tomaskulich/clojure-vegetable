(defproject vegetable "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [om "0.7.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha" :scope "provided"]
                 [org.clojure/clojurescript "0.0-2311"]
                 
                 ]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "vegetable"
              :source-paths ["src"]
              :compiler {
                :output-to "vegetable.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})

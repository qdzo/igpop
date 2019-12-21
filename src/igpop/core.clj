(ns igpop.core
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [igpop.site.core :as site]
            [clojure.set])
  (:gen-class))

(defmulti run (fn [nm & args] (keyword nm)))

(def commands
  {"help"     {:fn :help
               :desc "very concise usage guide"}
   "validate" {:fn :validate
               :desc "todo"}
   "build"    {:fn :build
               :desc "USAGE: igpop build {your_baseurl} EXAMPLE: igpop build /igpop"}
   "dev"      {:fn :dev
               :desc "-p to setup a port (default is 8899)"}})

(defmethod run
  :help
  [_ & args]
  (println "igpop [cmd] [subcmd] opts")
  (doseq [[k v] commands]
    (println " " k " - " (:desc v))))

(defmethod run
  :validate
  [& args]
  (println "Validate..." args))

(defmethod run
  :build
  [& args]
  (println "Build..." args)
  (site/build (System/getProperty "user.dir") (second args)))

(defmethod run
  :dev
  [& args]
  (println "Dev..." args)
  (let [i (.indexOf args "-p")
        port (last (butlast args))]
    (cond
      (and (> i 0) (not (= "-p" port)))
      (site/start (System/getProperty "user.dir") (Integer. port))
      (and (< i 0) (= "dev" port))
      (site/start (System/getProperty "user.dir") 8899)
      :else
      (run :help))))

(defn -main [& args]
  (if-let [cmd (first args)]
    (if-let [handler (get commands cmd)]
      (apply run args)
      (do
        (println "No such command - " cmd)
        (run :help)))
    (run :help)))

(comment

  (System/getProperty "user.dir")

  (-main "dev" "dir")


  )

;; -----------------------------------------------------------------------------------------

(defmulti run2 (fn [nm & args] (keyword nm)))

(defn help-msg []
  (str "igpop [cmd] [subcmd] opts\n"
       (->> (methods run2)
            (remove (comp #{:help :default} key))
            (map (fn [[k f]] (str k " - " (second (f)))))
            (interpose "\n")
            (apply str))))

(defmethod run2
  :help
  [_ & args]
  [:print "very concise usage guide"])

(help-msg)

(defmethod run2
  :default
  [& args]
  (if-let [cmd (first args)]
    [:print (str "No such command - " cmd "\n" (help-msg))]
    [:print (help-msg)]))

(defmethod run2
  :validate
  [& args]
  [:print (apply str "Validate..." args)])

(defmethod run2
  :build
  [& args]
  (if (second args)
    [:site/build (System/getProperty "user.dir") (second args)]
    [:print "USAGE: igpop build {your_baseurl} EXAMPLE: igpop build /igpop"]))

(defmethod run2
  :dev
  [& args]
  (let [i (.indexOf (or args []) "-p")
        port (last (butlast args))]
    (cond
      (and (> i 0) (not (= "-p" port)))
      [:site/start (System/getProperty "user.dir") (Integer. port)]
      (and (< i 0) (= "dev" port))
      [:site/start (System/getProperty "user.dir") 8899]
      :else
      [:print "-p to setup a port (default is 8899)"])))

(defn -main2 [& args]
  (run2 args))

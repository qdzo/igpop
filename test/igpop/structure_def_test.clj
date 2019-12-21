(ns igpop.structure-def-test
  (:require [igpop.structure-def :as sdef]
            [clojure.test :refer :all]
            [matcho.core :as matcho]
            [clojure.java.io :as io]
            [igpop.loader :as loader]
            [cheshire.core :refer :all]))

(deftest test-structure-gen

  (def profile
    {:Patient
     {:elements
      {:name {:type "HumanName"
              :required true
              :elements
              {:family {:type "string" :isCollection true :minItem 1 :maxItem 10}}}}}})


  (comment

    (testing "cardinality: entity `required`"
      (matcho/match
       (sdef/to-sd profile)
       {:snapshot [{:path "Patient.name" :min 1}]}))

    (testing "cardinality: entity `disabled`"
      (matcho/match
       (sdef/to-sd {:Patient {:elements {:name {:disabled true :type "HumanName"}}}})
       {:snapshot [{:path "Patient.name" :max 0}]}))


    (testing "cardinality: collection `min` and `max`"
      (matcho/match
       (sdef/to-sd profile)
       {:snapshot [{} {:path "Patient.name.family" :min 1 :max 10}]}))


    (testing "`mustSupport`: default  value"
      (matcho/match
       (sdef/to-sd profile)
       {:snapshot [{:mustSupport true}]}))


    (testing "`mustSupport`: override value"
      (matcho/match
       (sdef/to-sd {:Patient {:elements {:name {:type "HumanName" :mustSupport false}}}})
       {:snapshot [{:mustSupport false}]}))


    (testing "path: Correct `path`(nesting) construction"
      (matcho/match
       (sdef/to-sd profile)
       {:snapshot [{:path "Patient.name"} {:path "Patient.name.family"}]}))


    (testing "constant: correct key definition"
      (matcho/match
       (sdef/to-sd {:Patient {:elements {:name {:type "HumanName" :constant "Rich"}}}})
       {:snapshot [{:fixedHumanName "Rich"}]}))

    (def observation-profile
      {:Observation
       {:elements
        {:name
         {:constraints
          {:us-core-8 {:expression "family.exists() or given.exists()"
                       :description "Patient.name.given or Patient.name.family  or both SHALL be present"}}}}}})

    (testing "constraint: default value for `severity`"
      (matcho/match
       (sdef/to-sd observation-profile)
       {:snapshot [{:constraint [{:severity "error"}]}]}))

    (testing "constraint: correct key value"
      (matcho/match
       (sdef/to-sd observation-profile)
       {:snapshot [{:constraint [{:key "us-core-8"}]}]}))


    (testing "constraint: description -> human field"
      (matcho/match
       (sdef/to-sd observation-profile)
       {:snapshot [{:constraint
                    [{:human "Patient.name.given or Patient.name.family  or both SHALL be present"}]}]}))

    (testing "constraint: expression -> human field"
      (matcho/match
       (sdef/to-sd observation-profile)
       {:snapshot [{:constraint [{:expression  "family.exists() or given.exists()"
                                  :human  "Patient.name.given or Patient.name.family  or both SHALL be present"}]}]}))

    (def polymorphic-type
      {:Observation
       {:elements
        {:value {:required true
                 :union ["string" "CodeableConcept" "Quantity"]
                 :string {:required true}
                 :CodeableConcept {:required true
                                   :valueset {:id "vs"}}}}}})

    ;; (def sd-polymorphic-types
    ;;   [{:path "Observation.value[x]"
    ;;     :slicing {:discriminator {:type "type"}
    ;;               :path "$this"
    ;;               :ordered false
    ;;               :rules "closed"}
    ;;     :type [{:code "Quantity"},{:code "string"}]}
    ;;    {:path "Observation.valueCodeableConcept" :binding {:valuset "http://...."}}
    ;;    {:path "Observation.valueString" :required true}
    ;;    ])

    (testing "polymorphic: created item with `[x]` path"
      (matcho/match
       (sdef/to-sd polymorphic-type)
       {:snapshot [{:path "Observation.value[x]"}]}))


    (testing "polymorphic: created item `[x]` with correct type"
      (matcho/match
       (sdef/to-sd polymorphic-type)
       {:snapshot [{:type [{:code "string"},{:code "CodeableConcept"},{:code "Quantity"}]}]}))

    (testing "polymorphic: create items for union-types"
      (matcho/match
       (sdef/to-sd polymorphic-type)
       {:snapshot [{}]}))


    )



  (def project-path (.getPath (io/resource "test-project")))

  (def project (loader/load-project project-path))

  (comment

    (println project)

    (spit (io/file (str (System/getProperty "user.dir") "/show-project.json")) (generate-string project {:pretty true}))

    (println (sdef/generate-structure project))

    (get-in project [:profiles :Patient :basic :elements :name :elements :given])

    (spit (io/file (str (System/getProperty "user.dir") "/test-structure-def.json")) (generate-string (sdef/generate-structure project) {:pretty true}))

    ))


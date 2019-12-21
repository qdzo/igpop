(ns igpop.structure-def
  (:require [flatland.ordered.map :refer :all]))

(defn name-that-profile [rt prn] (str (name rt) (when (not (= "basic" (name prn)))
                                                  (str "_" (name prn)))))

(defn generate-snapshot [] (-> {}
                               (assoc :element [(-> {})])))

(defn element-processing [acc el parent-name] (-> {}
                                                  (assoc :id (str parent-name "." (name el)))))

(defn get-path
  [prefix key]
  (str prefix "." (name key)))

(defn flatten-profile
  [map prefix]
     (reduce
      (fn [acc [k v]]
        (if (map? v)
          (if (contains? v :elements)
            (merge (merge acc (ordered-map {(get-path prefix k) (dissoc v :elements)})) (flatten-profile (:elements v) (get-path prefix k)) )
            (merge acc (ordered-map {(get-path prefix k) v})))
          (merge acc (ordered-map {(get-path prefix k) v}))))
        (ordered-map []) map))

(defn generate-differential [rt prn props] (-> {}
                                               (assoc :element [(into (ordered-map []) (flatten-profile (:elements props) (name rt)))])))

(defn generate-structure [{diffs :diff-profiles profiles :profiles :as ctx}]
  (let [m {:resourceType "Bundle"
           :id "resources"
           :meta {:lastUpdated (java.util.Date.)}
           :type "collection"}]
    (assoc m :entry
           (into [] (apply concat (for [[rt prls] diffs]
                                    (for [[prn props] prls]
                                      (-> {}
                                          (assoc :fullUrl (str "baseUrl" "/" (name-that-profile rt prn)))
                                          (assoc :resource (-> {}
                                                               (assoc :resourceType "StructureDefinition")
                                                               (assoc :id (name prn))
                                                               (assoc :description (:description props))
                                                               (assoc :type (name rt))
                                                               (assoc :snapshot (generate-snapshot))
                                                               (assoc :differential (generate-differential rt prn props))))))))))))


;; ----------------------------------------------------------------------------------------

(require '[clojure.string :as s])

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------

(defn capitalize
  "Uppercase first character of the string.
  Unlike `clojure.string/capitalize` - we don't lowercase rest of the string"
  [^String s]
  (str (.toUpperCase (subs s 0 1)) (subs s 1)))

(defn to-sd-path [parts]
  (s/join "." (map name parts)))

(defn poly-name-to-path-x
  "Convert path `parts` to path-x of polymorphic value
  Example: `[:A,:B,:C] -> A.B.C[x]`"
  [path] (str (to-sd-path path) "[x]"))

(defn poly-name-to-path
  "Convert path `parts` and `poly-value` to path of polymorphic value
  Example: `[:A,:B,:C] and value -> A.B.CValue`"
  [path poly-value]
  (str (to-sd-path path) (capitalize (name poly-value))))

;; ---------------------------------------------------------------------


;; ---------------------------------------------------------------------

;; Convert igpop Polymorphic types
;; ```
;; elements:
;;   value:
;;     required: true
;;     union: [string CodeableConcept Quantity]
;;     string: {required: true}
;;     CodeableConcept:
;;       required: true
;;       valueset {id: 'vs'}
;; ```
;;
;; into several structure-definition polymorphic entries
;;
;; ```
;; - path: Observation.value[x]
;;   slicing:
;;     discriminator:
;;     - type: type
;;     path: "$this"
;;     ordered: false
;;     rules: closed
;;   type:
;;   - code: Quantity
;;   - code: string
;; - path: Observation.valueCodeableConcept
;;   binding: {valuset: 'http://....'}
;; ...
;; - path: Observation.valueString
;; ```

(defn igpop-polymorphic->sd-polymorphic [{:keys [path] :as ctx} item-name item]
  (let [union-types (:union item)
        union-defined (select-keys item (mapv keyword union-types))
        new-path (conj path item-name)
        poly-value-item (fn [[poly-name item]]
                          (cond-> (assoc item :path (poly-name-to-path new-path poly-name))
                            ;; what keys also need put into `binding`?
                            (:valueset item) (-> (assoc :binding (select-keys item [:valueset]))
                                                 dissoc :valuset)))]
    (vec (conj (map poly-value-item union-defined)
               {:type (mapv (fn [t] {:code t}) union-types)
                :path (poly-name-to-path-x new-path)
                :slicing {:discriminator {:type "type"}
                          :path "$this"
                          :ordered false
                          :rules "closed"}}))))

;; (assert (=
;;          (igpop-polymorphic->sd-polymorphic
;;           {:path [:Observation]}
;;           (key (first igpop-polymorphic-type))
;;           (val (first igpop-polymorphic-type)))
;;          sd-polymorphic-types))


;; convert igpop constraints
;; ```
;; elements:
;;   name:
;;     constaints:
;;       us-core-8:
;;         expression: "family.exists() or given.exists()"
;;         description: "Patient.name.given or Patient.name.family or both SHALL be present"
;;         # severity: error (default)
;; ```
;;
;; into structure-definition constraints
;;
;; ```
;; constraints:
;; - severity: error
;;   key: us-core-8
;;   expression: "family.exists() or given.exists()"
;;   human: "Patient.name.given or Patient.name.family or both SHALL be present"
;; ```
(defn ig-constraint->sd-constraint
  [[item-name item :as constraint]]
  {:severity   (or (:severity item) "error")
   :key        (name item-name)
   :expression (:expression item)
   :human      (:description item)})


;; convert `ig-pop-item` to `structure-definition-item`
;; Cardinality-patterns should be checked earlier
;; we don't check XOR (exactly one) and try to find any of them: (and override values)
;;   - disabled true
;;   - required true
;;   - min and max
(defn ig-item->sd-item [ctx item-name item]
  (let [{:keys [mustSupport type
                disabled required minItem
                maxItem constant constraints]} item]
    ;; can use `transients` for optimizations. But is there sense?
    (cond-> {:path (to-sd-path (conj (:path ctx) item-name))
             :mustSupport true
             :type type}
      (= false mustSupport) (assoc :mustSupport mustSupport)
      disabled    (assoc :max 0)
      required    (assoc :min 1)
      minItem     (assoc :min minItem)
      maxItem     (assoc :max maxItem)
      constant    (assoc (keyword (str "fixed" type)) constant)
      constraints (assoc :constraint (mapv ig-constraint->sd-constraint constraints))
      )))

;; ---------------------------------------------------------------------

;; walk on elements
;; converts item to structure definitions
;; if there are some nested elements, go deeper
;;
;; returns context:
;;  {:path [path of current position]
;;   :result [converted elements]}
(defn to-sd-elements
  [ctx elements]
  (reduce (fn [ctx [item-name item]]
            (cond-> ctx
              (:union item)
              (update :result into (igpop-polymorphic->sd-polymorphic ctx item-name item))

              ;; not a union and not top-level item
              (and (not (:union item))
                   (not= (:path ctx) []))
              (update :result conj (ig-item->sd-item ctx item-name item))

              (:elements item)
              (-> (update :path conj item-name) (to-sd-elements (:elements item)))

              ;; reset path before next iteration
              :default            (assoc :path (:path ctx))))
          ctx elements))

;; reset path to original version (:elements add parts to path)
;; ---------------------------------------------------------------------

(defn to-sd
  "Transform igpop nested structure to flat vector of
  structure-definitions and wraps them in `{:snapshot [definitions]}`"
  [igpop]
  {:snapshot (-> {:path [] :result []}
                 (to-sd-elements igpop)
                 :result)})

;; (to-sd-path [:Patient :name])

(def profile
  {:Patient
   {:elements
    {:name {:type "HumanName"
            :minItem 1
            :constant "Viktoria"
            :elements
            {:family {:type "string" :isCollection true :minItem 1 :maxItem 10}}}}}
   :Patient2
   {:elements
    {:name {:type "HumanName"
            :minItem 1
            :constant  "Viktoria"
            :mustSupport false
            :elements
            {:family {:type "string" :isCollection true :minItem 1 :maxItem 10}}}
     :coding {:type "Coding"
              :constant {:code "code-1", :system "sys-1"}}}
    }
   })

(def igpop-polymorphic-type
  {:value
   {:required true
    :union ["string" "CodeableConcept" "Quantity"]
    :string {:required true}
    :CodeableConcept {:required true
                      :valueset {:id "vs"}}}})

(def sd-polymorphic-types
  [{:path "Observation.value[x]"
    :slicing {:discriminator {:type "type"}
              :path "$this"
              :ordered false
              :rules "closed"}
    :type [{:code "Quantity"},{:code "string"}]}
   {:path "Observation.valueCodeableConcept" :binding {:valuset "http://...."}}
   {:path "Observation.valueString" :required true}
   ])




(->> (to-sd profile)
    :snapshot
    ;; (map :path)
    ;; second
    )


;; =======================================================================

;; extended parsing version

(def rules
  {#{:elements}           :nested-structure
   #{:minItems :maxItems
     :required :disabled} :cardinality
   #{:constraints}        :constraint
   #{:constant}           :constant
   })

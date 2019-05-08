(define-resource alert ()
  :class (s-prefix "ext:SysteemNotificatie") ;; NOTE: as well as skos:Concept
  :properties `((:begin-date 	:datetime ,(s-prefix "ext:beginDatum"))
                (:end-date 		:datetime ,(s-prefix "ext:eindDatum"))
								(:title 			:string 	,(s-prefix "ext:titel"))
								(:message 		:string 	,(s-prefix "ext:bericht")))
  :has-one `((alert-type 			:via 			,(s-prefix "ext:type") ;; NOTE: What is the domain of Besluit geeftAanleidingTot? guessed prov:generated
                         			:as "type"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/alerts/")
  :features '(include-uri)
  :on-path "alerts")

(define-resource alert-type ()
  :class (s-prefix "ext:SysteemNotificatieType") ;; NOTE: as well as skos:Concept
  :properties `((:label 			:string 	,(s-prefix "skos:prefLabel"))
                (:scope-note 	:string 	,(s-prefix "skos:scopeNote"))
                (:alt-label :string ,(s-prefix "skos:altLabel")))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/concept/alert-types/")
  :features '(include-uri)
  :on-path "alert-types")

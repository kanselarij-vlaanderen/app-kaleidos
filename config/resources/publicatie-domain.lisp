(define-resource publication-flow ()
  :class (s-prefix "pub:Publicatieaangelegenheid")
  :properties `((:publication-number  :string   ,(s-prefix "pub:publicatieNummer"))
                (:translate-before    :datetime ,(s-prefix "pub:uitersteVertaling")) ;; in de subcase ?
                (:publish-before      :datetime ,(s-prefix "pub:uiterstePublicatie")) ;; in de subcase ?
                (:published-at        :datetime ,(s-prefix "pub:publicatieOp")) ;; in de subcase ?                per drukproef/publicatie ??
                (:publication-method  :url      ,(s-prefix "pub:publicatieWijze")) ;; url of publicatiewijze      per drukproef/publicatie ??
                (:numac-number        :string   ,(s-prefix "pub:numacNummer")) ;; object ? number ?  numac nummer per drukproef/publicatie ??
                (:remark              :string   ,(s-prefix "pub:publicatieOpmerking")) ;; check of dit nodig is
                (:created             :datetime ,(s-prefix "dct:created"))
                (:modified            :datetime ,(s-prefix "dct:modified")))
  :has-one `((case                    :via      ,(s-prefix "dossier:behandelt") ;; gaan we bestaande case gebruiken hiervoor? is niet voor kanselarij
                                      :as "case")
             (publication-status      :via      ,(s-prefix "pub:publicatiestatus")
                                      :as "status"))
  :has-many `((subcase                :via      ,(s-prefix "ext:doorloopt") ;; dossier:doorloopt kan niet, mu-cl-resources
                                      :as "subcases")
              (person                 :via      ,(s-prefix "pub:contactpersoon")
                                      :as "contacts"))
              ;; mandatees van subcase bij MR,  ongekende mandatee bij niet via MR
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/publicatie-aangelegenheden/")
  :features `(include-uri)
  :on-path "publication-flows")

(define-resource publication-status ()
  :class (s-prefix "pub:Publicatiestatus")
  :properties `((:name        :string ,(s-prefix "dct:title"))
                (:priority    :number ,(s-prefix "ext:priority")
  :has-many `((publication-flow    :via    ,(s-prefix "pub:publicatiestatus")
                              :inverse t
                              :as "publicaties"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/publication-statussen/")
  :features '(include-uri)
  :on-path "publication-statuses")

(define-resource language ()
  :class (s-prefix "ext:Taal")
  :properties `((:name                 :string ,(s-prefix "dct:title")
                (:iso-language-code    :string ,(s-prefix "dct:language")
                (:priority             :number ,(s-prefix "ext:priority"))
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/talen/")
  :features '(include-uri)
  :on-path "languages")


  ;; 
  ;; historiek ? momenteel weg gehaald, mogelijk later terug
  ;; aangevraagd door, instantie, heeft ook besluitdomeinen
  ;; persoon wie opmerking gemaakt heeft lijkt niet nodig
  ;; laatste wijziging door wie ?
  ;; create BY ?


  ;; done
  ;; status publicatie
  ;; uiterste vertaaldatum
  ;; uiterste datum publicatie
  ;; werknummer BS / numac
  ;; publicatie datum in BS
  ;; wijze van publicate (type)
  ;; laatste wijziging
  ;; verstuurd naar BS ?
  ;; vertalingen krijgen fysiek een bestand, model maken paralell op serie? meerdere pieces die gelinkt zijn aan 1 iets, alle files zijn ofwel andere extensie of vertaald.
  ;; handtekeningen gebeuren op de PDF, vertalingen op de word
  ;; documenten (pdf en word aan elkaar kunnen hangen), opsplitsen ? 
  ;; contact persoon, dit is niet per se een bestaande user, gewoon text ?
  ;; opmerking 


  

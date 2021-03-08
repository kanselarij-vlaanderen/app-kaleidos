(define-resource user ()
  :class (s-prefix "foaf:Person")
  :resource-base (s-url "http://themis.vlaanderen.be/id/gebruikers/")
  :properties `((:first-name            :string   ,(s-prefix "foaf:firstName"))
                (:last-name             :string   ,(s-prefix "foaf:familyName"))
                (:email-link            :uri      ,(s-prefix "foaf:mbox"))
                (:phone-link            :uri      ,(s-prefix "foaf:phone")))
  :has-one `((account-group             :via      ,(s-prefix "foaf:member")
                                        :inverse t
                                        :as "group")
             (account                   :via      ,(s-prefix "foaf:account")
                                        :as "account")
             (organization              :via      ,(s-prefix "org:memberOf")
                                        :as "organization")
             ) ;; Has one relation adms:identifier to type adms::Identifier created in user-management-service
  :on-path "users"
)

(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :resource-base (s-url "http://themis.vlaanderen.be/id/accounts/")
  :properties `((:provider     :via ,(s-prefix "foaf:accountServiceHomepage"))
                (:vo-id        :via ,(s-prefix "dct:identifier"))
                (:vo-email     :via ,(s-prefix "ext:email"))
                (:phone-number :via ,(s-prefix "ext:phone")))
  :has-one `((user            :via ,(s-prefix "foaf:account")
                              :inverse t
                              :as "user"))
  :on-path "accounts"
)

(define-resource account-group ()
  :class (s-prefix "foaf:Group")
  :resource-base (s-url "http://themis.vlaanderen.be/id/account-groups/")
  :properties `((:name  :via ,(s-prefix "foaf:name")))
  :has-many `((user     :via ,(s-prefix "foaf:member")
                        :as "users"))
  :on-path "account-groups"
)

(define-resource user ()
  :class (s-prefix "foaf:Person")
  :resource-base (s-url "http://localhost/id/gebruiker/")
  :properties `((:first-name :string ,(s-prefix "foaf:firstName"))
                (:last-name :string ,(s-prefix "foaf:familyName"))
                (:rijksregister-nummer :string ,(s-prefix "dct:identifier")))
  :has-many `((account :via ,(s-prefix "foaf:account")
                       :as "account")
              (account-group :via ,(s-prefix "foaf:member")
                             :inverse t
                             :as "groups"))
  :on-path "users"
)

(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :resource-base (s-url "http://localhost/id/account/")
  :properties `((:provider :via ,(s-prefix "foaf:accountServiceHomepage"))
                (:vo-id :via ,(s-prefix "dct:identifier")))
  :has-one `((user :via ,(s-prefix "foaf:account")
                         :inverse t
                         :as "user"))
  :on-path "accounts"
)


(define-resource account-group ()
  :class (s-prefix "foaf:Group")
  :resource-base (s-url "http://localhost/id/account-group/")
  :properties `((:name :via ,(s-prefix "foaf:name")))
  :has-many `((user :via ,(s-prefix "foaf:member")
                    :as "users"))
  :on-path "account-groups"
)

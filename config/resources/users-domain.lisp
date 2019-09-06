(define-resource user ()
  :class (s-prefix "foaf:Person")
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/gebruikers/")
  :properties `((:first-name            :string   ,(s-prefix "foaf:firstName"))
                (:last-name             :string   ,(s-prefix "foaf:familyName"))
                (:rijksregister-nummer  :string   ,(s-prefix "dct:identifier")))
  :has-one `((account-group             :via      ,(s-prefix "foaf:member")
                                        :inverse t
                                        :as "group")
             (account                   :via      ,(s-prefix "foaf:account")
                                        :as "account"))
  :on-path "users"
)

(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/accounts/")
  :properties `((:provider    :via ,(s-prefix "foaf:accountServiceHomepage"))
                (:vo-id       :via ,(s-prefix "dct:identifier")))
  :has-one `((user            :via ,(s-prefix "foaf:account")
                              :inverse t
                              :as "user"))
  :on-path "accounts"
)

(define-resource account-group ()
  :class (s-prefix "foaf:Group")
  :resource-base (s-url "http://kanselarij.vo.data.gift/id/account-groups/")
  :properties `((:name  :via ,(s-prefix "foaf:name")))
  :has-many `((user     :via ,(s-prefix "foaf:member")
                        :as "users"))
  :on-path "account-groups"
)

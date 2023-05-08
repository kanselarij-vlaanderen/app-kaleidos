alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.Accessibility.ByQuery, as: AccessByQuery
alias Acl.GraphSpec.Constraint.Resource.AllPredicates, as: AllPredicates
alias Acl.GraphSpec.Constraint.Resource.NoPredicates, as: NoPredicates
alias Acl.GraphSpec.Constraint.ResourceFormat, as: ResourceFormatConstraint
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup

defmodule Acl.UserGroups.Config do
  defp admin_roles do
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/9a969b13-e80b-424f-8a82-a402bcb42bc5>" # admin
    ]
  end

  defp secretarie_roles do
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/ab39b02a-14a5-4aa9-90bd-e0fa268b0f3d>", # kanselarij
      "<http://themis.vlaanderen.be/id/gebruikersrol/c2ef1785-bf28-458f-952d-aa40989347d2>" # secretarie
    ]
  end

  defp ovrb_roles do
    # kanselarij_role is explicitely not added here, to avoid the requirement of
    # additional search indexes because of the new combination of user-groups.
    # Since 'ovrb' data access is a subset of 'secretarie' data access,
    # it's sufficient to add kanselarij_role only to secretarie_roles()
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/648a1ffe-1a26-4931-a329-18d26a91438f>" # ovrb
    ]
  end

  defp kort_bestek_roles do
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/ca20a872-7743-4998-b479-06b003f49daf>" # kort bestek
    ]
  end

  defp minister_roles do
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/01ace9e0-f810-474e-b8e0-f578ff1e230d>" # minister and kabinetchef
    ]
  end

  defp kabinet_dossierbeheerder_roles do
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/6bcebe59-0cb5-4c5e-ab40-ca98b65887a4>", # kabinet dossierbeheerder
    ]
  end

  defp kabinet_medewerker_roles do
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/33dbca4a-7e57-41d2-a26c-aedef422ff84>" # kabinet medewerker
    ]
  end

  defp overheid_roles do
    [
      "<http://themis.vlaanderen.be/id/gebruikersrol/06cfd67b-1637-47d3-811f-97aa23a83644>", # overheidsorganisatie
      "<http://themis.vlaanderen.be/id/gebruikersrol/12543581-7f02-4166-87d2-ab15ddfce642>" # Vlaams Parlement
    ]
  end

  defp access_by_authenticated() do
    %AccessByQuery{
      vars: [],
      query: "PREFIX org: <http://www.w3.org/ns/org#>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              SELECT ?role_uri WHERE {
                <SESSION_ID> ext:sessionMembership / org:role ?role_uri .
              } LIMIT 1"
    }
  end

  defp access_by_role(role_uris) do
    %AccessByQuery{
      vars: [],
      query: "PREFIX org: <http://www.w3.org/ns/org#>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              SELECT ?role_uri WHERE {
                <SESSION_ID> ext:sessionMembership / org:role ?ownRole .
                OPTIONAL { <SESSION_ID> ext:impersonatedRole ?maybeImpersonatedRole . }
                BIND(COALESCE(?maybeImpersonatedRole, ?ownRole) AS ?role_uri)
                VALUES ?role_uri { #{Enum.join(role_uris, " ")} }
              } LIMIT 1"
    }
  end

  defp access_by_own_role(role_uris) do
    %AccessByQuery{
      vars: [],
      query: "PREFIX org: <http://www.w3.org/ns/org#>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              SELECT ?role_uri WHERE {
                <SESSION_ID> ext:sessionMembership / org:role ?role_uri .
                VALUES ?role_uri { #{Enum.join(role_uris, " ")} }
              } LIMIT 1"
    }
  end

  defp generic_besluitvorming_resource_types() do
    [
      "https://data.vlaanderen.be/ns/dossier#Dossier",
      "https://data.vlaanderen.be/ns/besluitvorming#Besluitvormingsaangelegenheid",
      "https://data.vlaanderen.be/ns/dossier#Procedurestap"
    ]
  end

  defp document_resource_types() do
    [
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
      "http://xmlns.com/foaf/0.1/Document", # TODO: Delete after complete document migration, still data on PROD!
      "https://data.vlaanderen.be/ns/dossier#Serie",
      "http://mu.semte.ch/vocabularies/ext/DocumentVersie", # TODO: Delete after complete document migration, still data on PROD!
      "https://data.vlaanderen.be/ns/dossier#Stuk"
    ]
  end

  defp agendering_resource_types() do
    [
      "https://data.vlaanderen.be/ns/besluitvorming#Agenda",
      "http://data.vlaanderen.be/ns/besluit#Agendapunt",
      "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
      "http://www.w3.org/ns/prov#Activity",
      "https://data.vlaanderen.be/ns/besluitvorming#Beslissingsactiviteit",
      "http://data.vlaanderen.be/ns/besluit#Vergaderactiviteit",
      "https://data.vlaanderen.be/ns/besluitvorming#Agendering",
      "http://mu.semte.ch/vocabularies/ext/Indieningsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/AgendaStatusActivity"
    ]
  end

  defp file_bundling_resource_types() do
    [
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
      "http://www.w3.org/ns/prov#Collection",
      "http://vocab.deri.ie/cogs#Job",
      "http://mu.semte.ch/vocabularies/ext/FileBundlingJob"
    ]
  end

  defp publication_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/publicatie/Publicatieaangelegenheid",
      "http://mu.semte.ch/vocabularies/ext/publicatie/VertalingProcedurestap",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicatieProcedurestap",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicatieStatusWijziging",
      "http://www.w3.org/ns/person#Person",
      "http://schema.org/ContactPoint",
      "http://www.w3.org/ns/org#Organization",
      "http://www.w3.org/ns/prov#Activity",
      "http://mu.semte.ch/vocabularies/ext/publicatie/AanvraagActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/VertaalActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/DrukproefActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicatieActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/AnnulatieActiviteit",
      "https://data.vlaanderen.be/ns/generiek#GestructureerdeIdentificator",
      "http://www.w3.org/ns/adms#Identifier",
      "https://data.vlaanderen.be/ns/besluitvorming#Beslissingsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicationMetricsExportJob",
      "http://data.europa.eu/eli/ontology#LegalResource" # manual registration of decisions
    ]
  end

  defp sign_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Handtekenaangelegenheid",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/HandtekenProcedurestap",
      "http://www.w3.org/ns/prov#Activity",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Markeringsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Voorbereidingsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Handtekenactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Goedkeuringsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Weigeractiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/AnnulatieActiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Afrondingsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/signinghub/Document",
      "http://www.w3.org/ns/person#Person",
      "https://data.vlaanderen.be/ns/besluitvorming#Beslissingsactiviteit"
    ]
  end

  defp staatsblad_resource_types() do
    [
      "http://data.europa.eu/eli/ontology#LegalResource"
    ]
  end

  defp email_resource_types() do
    [
      # "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#Mailbox",
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#Folder",
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#Email",
      "http://mu.semte.ch/vocabularies/ext/EmailNotificationSettings"
      # "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#MessageHeader",
    ]
  end

  defp newsletter_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/Nieuwsbericht",
      "http://mu.semte.ch/vocabularies/ext/MailCampagne",
      "http://www.w3.org/ns/prov#Activity",
      "http://mu.semte.ch/vocabularies/ext/InternalDecisionPublicationActivity",
      "http://mu.semte.ch/vocabularies/ext/InternalDocumentPublicationActivity",
      "http://mu.semte.ch/vocabularies/ext/ThemisPublicationActivity"
    ]
  end

  defp user_account_resource_types() do
    [
      "http://xmlns.com/foaf/0.1/OnlineAccount",
      "http://xmlns.com/foaf/0.1/Person",
      "http://xmlns.com/foaf/0.1/Organization",
      "http://www.w3.org/ns/org#Membership"
    ]
  end

  defp user_activity_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/LoginActivity"
    ]
  end

  defp themis_export_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/PublicExportJob",
      "http://mu.semte.ch/vocabularies/ext/TtlToDeltaTask"
    ]
  end

  defp system_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatie"
    ]
  end

  defp public_static_data() do
    [
      "http://data.vlaanderen.be/ns/mandaat#Mandaat",
      "http://data.vlaanderen.be/ns/mandaat#Mandataris",
      "http://www.w3.org/ns/person#Person", # when used as bestuurlijke-alias-van mandaat:Mandataris
      "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
      "http://data.vlaanderen.be/ns/besluit#Bestuursorgaan",
      "http://www.w3.org/ns/prov#Generation",
      "http://www.w3.org/ns/prov#Invalidation",
      "http://www.w3.org/ns/org#Organization"
    ]
  end

  defp public_codelists() do
    [
      "http://mu.semte.ch/vocabularies/ext/ThemaCode",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatieType",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapType",
      "http://mu.semte.ch/vocabularies/ext/publicatie/Publicatiestatus",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicatieWijze",
      "http://mu.semte.ch/vocabularies/ext/publicatie/Urgentieniveau",
      "http://mu.semte.ch/vocabularies/ext/publicatie/Publicatierapporttype",
      "http://mu.semte.ch/vocabularies/ext/RegelgevingType",
      "http://publications.europa.eu/ontology/euvoc#Language",
      "http://www.w3.org/ns/org#Role",
      "http://www.w3.org/2004/02/skos/core#Concept",
      "http://www.w3.org/2004/02/skos/core#ConceptScheme",
      "http://mu.semte.ch/vocabularies/ext/DocumentType"
    ]
  end

  def user_groups do
    # These elements are walked from top to bottom.  Each of them may
    # alter the quads to which the current query applies.  Quads are
    # represented in three sections: current_source_quads,
    # removed_source_quads, new_quads.  The quads may be calculated in
    # many ways.  The useage of a GroupSpec and GraphCleanup are
    # common.
    [
      %GroupSpec{
        name: "public",
        useage: [:read],
        access: %AlwaysAccessible{},
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/public",
            constraint: %ResourceConstraint{
              resource_types: public_static_data() ++
              public_codelists() ++
              system_resource_types() ++
              user_account_resource_types() # required to list mock accounts for unauthenticated users
            } },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/sessions",
            constraint: %ResourceFormatConstraint{
              resource_prefix: "http://mu.semte.ch/sessions/"
            } },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/staatsblad",
            constraint: %ResourceConstraint{
              resource_types: staatsblad_resource_types()
            } }, 
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/themis-public",
            constraint: %ResourceConstraint{
              resource_types: themis_export_types()
            } } ]
      },
      %GroupSpec{
        name: "authenticated",
        useage: [:read],
        access: access_by_authenticated(),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/users",
            constraint: %ResourceConstraint{
              resource_types: user_account_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "admin",
        useage: [:read, :write, :read_for_write],
        access: access_by_own_role(admin_roles()),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/sessions",
            constraint: %ResourceFormatConstraint{
              resource_prefix: "http://mu.semte.ch/sessions/"
            }
          },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/users",
            constraint: %ResourceConstraint{
              resource_types: user_account_resource_types() ++ user_activity_types()
            }
          },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/public",
            constraint: %ResourceConstraint{
              resource_types: system_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "secretarie",
        useage: [:read, :write, :read_for_write],
        access: access_by_role(admin_roles() ++ secretarie_roles() ++ kort_bestek_roles()),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/kanselarij",
            constraint: %ResourceConstraint{
              resource_types: newsletter_resource_types() ++
                agendering_resource_types() ++
                generic_besluitvorming_resource_types() ++
                document_resource_types() ++
                file_bundling_resource_types() ++
                publication_resource_types()
            }
          },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/email",
            constraint: %ResourceConstraint{
              resource_types: email_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "ovrb",
        useage: [:read, :write, :read_for_write],
        # TODO: Read access on whole "kanselarij"-graph for now.
        access: access_by_role(admin_roles() ++ ovrb_roles()),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/kanselarij",
            constraint: %ResourceConstraint{
              resource_types: generic_besluitvorming_resource_types() ++
                document_resource_types() ++
                publication_resource_types()
            }
          },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/email",
            constraint: %ResourceConstraint{
              resource_types: email_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "o-minister-read",
        useage: [:read, :write, :read_for_write],
        access: access_by_role(
          minister_roles()
          ++ kabinet_dossierbeheerder_roles() # Technically this spec is too broad for dossierbeheerders, but we add extra checks down the line
        ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/minister",
            constraint: %ResourceConstraint{
              resource_types: file_bundling_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "o-intern-regering-read",
        useage: [:read, :write, :read_for_write],
        access: access_by_role(kabinet_medewerker_roles()),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/intern-regering",
            constraint: %ResourceConstraint{
              resource_types: file_bundling_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "o-intern-overheid-read",
        useage: [:read, :write, :read_for_write],
        access: access_by_role(overheid_roles()),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/intern-overheid",
            constraint: %ResourceConstraint{
              resource_types: file_bundling_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "sign-flow-read",
        useage: [:read],
        access: access_by_role(
          admin_roles()
          ++ secretarie_roles()
          ++ kort_bestek_roles()
          ++ minister_roles()
          ++ kabinet_dossierbeheerder_roles()
          ++ kabinet_medewerker_roles()
        ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/signing",
            constraint: %ResourceConstraint{
              resource_types: sign_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "sign-flow-write",
        useage: [:write, :read_for_write],
        access: access_by_role(
          admin_roles()
          ++ secretarie_roles()
          ++ kort_bestek_roles()
          ++ minister_roles()
          ++ kabinet_dossierbeheerder_roles()
        ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/signing",
            constraint: %ResourceConstraint{
              resource_types: sign_resource_types()
            }
          }
        ]
      },

      # // READ ACCESS FOR SYNC-CONSUMER SERVICE FROM OTHER STACK
      #
      %GroupSpec{
        name: "sync-consumer",
        useage: [:read],
        access: %AccessByQuery{
          vars: [],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

          SELECT ?thing WHERE {
            <SESSION_ID> muAccount:account <http://services.lblod.info/diff-consumer/account>.
            VALUES ?thing { \"let me in\" }
          }"
        },
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/delta-files",
            constraint: %ResourceConstraint{
              resource_types: [
                "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject"
              ]
            }
          }
        ]
      },

      # // CLEANUP
      #
      %GraphCleanup{
        originating_graph: "http://mu.semte.ch/application",
        useage: [:write],
        name: "clean"
      }
    ]
  end
end

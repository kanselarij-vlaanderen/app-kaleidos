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

  defp access_by_group( group_uris ) do
    %AccessByQuery{
      vars: [],
      query: "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
              PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              SELECT ?group_uri WHERE {
                <SESSION_ID> session:account / ^foaf:account / ^foaf:member ?group_uri .
                VALUES ?group_uri { #{group_uris} }
              } LIMIT 1"
    }
  end

  defp generic_besluitvorming_resource_types() do
    [
      "https://data.vlaanderen.be/ns/dossier#Dossier",
      "https://data.vlaanderen.be/ns/dossier#Procedurestap",
      "http://www.w3.org/ns/prov#Activity", # TODO: do we still need this for later with polymorphism ?
    ]
  end

  defp document_resource_types() do
    [
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
      "http://xmlns.com/foaf/0.1/Document", # TODO: Delete after complete document migration, still data on PROD!
      "https://data.vlaanderen.be/ns/dossier#Serie",
      "http://mu.semte.ch/vocabularies/ext/DocumentVersie", # TODO: Delete after complete document migration, still data on PROD!
      "https://data.vlaanderen.be/ns/dossier#Stuk",
    ]
  end

  defp agendering_resource_types() do
    [
      "http://data.vlaanderen.be/ns/besluitvorming#Agenda",
      "http://data.vlaanderen.be/ns/besluit#Agendapunt",
      "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
      "http://data.vlaanderen.be/ns/besluit#Vergaderactiviteit",
      "http://data.vlaanderen.be/ns/besluitvorming#Agendering",
      "http://mu.semte.ch/vocabularies/ext/Indieningsactiviteit",
    ]
  end

  defp file_bundling_resource_types() do
    [
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
      "http://www.w3.org/ns/prov#Collection",
      "http://vocab.deri.ie/cogs#Job",
      "http://mu.semte.ch/vocabularies/ext/FileBundlingJob",
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
      "http://mu.semte.ch/vocabularies/ext/publicatie/AanvraagActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/VertaalActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/DrukproefActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicatieActiviteit",
      "http://mu.semte.ch/vocabularies/ext/publicatie/AnnulatieActiviteit",
      "https://data.vlaanderen.be/ns/generiek#GestructureerdeIdentificator",
      "http://www.w3.org/ns/adms#Identifier",
      "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicationMetricsExportJob",
    ]
  end


  defp sign_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Handtekenaangelegenheid",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/HandtekenProcedurestap",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Markeringsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Voorbereidingsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Handtekenactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Weigeractiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/AnnulatieActiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/Afrondingsactiviteit",
      "http://mu.semte.ch/vocabularies/ext/handtekenen/GetekendStuk",
      "http://mu.semte.ch/vocabularies/ext/signinghub/Document",
      "http://www.w3.org/ns/person#Person",
      "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
    ]
  end

  defp staatsblad_resource_types() do
    [
      "http://data.europa.eu/eli/ontology#LegalResource",
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
      "http://data.vlaanderen.be/ns/besluitvorming#NieuwsbriefInfo",
      "http://mu.semte.ch/vocabularies/ext/MailCampagne",
      "http://mu.semte.ch/vocabularies/ext/ThemisPublicationActivity"
    ]
  end

  defp user_account_resource_types() do
    [
      "http://xmlns.com/foaf/0.1/OnlineAccount",
      "http://xmlns.com/foaf/0.1/Person",
      "http://xmlns.com/foaf/0.1/Group",
      "http://www.w3.org/ns/adms#Identifier",
    ]
  end

  # Also insert your type as ext:PublicClass
  defp unconfidential_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/DocumentIdentifier", # TODO: check if this type is in use.
      "http://data.vlaanderen.be/ns/mandaat#Mandaat",
      "http://data.vlaanderen.be/ns/mandaat#Mandataris",
      "http://www.w3.org/ns/person#Person",
      "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
      "http://data.vlaanderen.be/ns/besluit#Bestuursorgaan",
      "http://www.w3.org/ns/prov#Generation",
      "http://www.w3.org/ns/prov#Invalidation",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatie",
      "http://www.w3.org/ns/org#Organization",
    ]
  end
  # Also insert your type as ext:PublicClass

  # Also insert your type as ext:PublicClass
  defp static_unconfidential_code_list_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/DocumentTypeCode",
      "http://mu.semte.ch/vocabularies/ext/ThemaCode",
      "http://mu.semte.ch/vocabularies/ext/Thema", # TODO: check if this type is in use. Looks like only "ThemaCode" is.
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatieType",
      "http://mu.semte.ch/vocabularies/ext/BeslissingsResultaatCode",
      "http://mu.semte.ch/vocabularies/ext/DossierTypeCode",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapType",
      "http://mu.semte.ch/vocabularies/ext/publicatie/Publicatiestatus",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicatieWijze",
      "http://mu.semte.ch/vocabularies/ext/publicatie/Urgentieniveau",
      "http://mu.semte.ch/vocabularies/ext/RegelgevingType",
      "http://publications.europa.eu/ontology/euvoc#Language",
      "http://www.w3.org/2004/02/skos/core#Concept",
      "http://www.w3.org/2004/02/skos/core#ConceptScheme",
      "http://mu.semte.ch/vocabularies/ext/publicatie/PublicationMetricsExportType",
    ]
  end
  # Also insert your type as ext:PublicClass

  def user_groups do
    # These elements are walked from top to bottom.  Each of them may
    # alter the quads to which the current query applies.  Quads are
    # represented in three sections: current_source_quads,
    # removed_source_quads, new_quads.  The quads may be calculated in
    # many ways.  The useage of a GroupSpec and GraphCleanup are
    # common.
    [
      # // PUBLIC TODO for now public is the same as privileged and privileged is therefore not created
      %GroupSpec{
        name: "public",
        useage: [:read],
        access: %AlwaysAccessible{}, # TODO: Should be only for logged in users
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/public",
            constraint: %ResourceConstraint{
              resource_types: unconfidential_resource_types() ++
              static_unconfidential_code_list_types() ++
              user_account_resource_types()
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
            } } ]
      },
      %GroupSpec{
        name: "o-intern-overheid-read",
        useage: [:read, :write, :read_for_write],
        access: access_by_group( "<http://data.kanselarij.vlaanderen.be/id/group/overheid>" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/intern-overheid",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"] ++file_bundling_resource_types()
            }
          }
        ]
      },

      %GroupSpec{
        name: "writes-on-public",
        useage: [:write, :read_for_write],
        access: access_by_group( "<http://data.kanselarij.vlaanderen.be/id/group/admin>
                                 <http://data.kanselarij.vlaanderen.be/id/group/kanselarij>"),
        graphs: [ %GraphSpec{
          graph: "http://mu.semte.ch/graphs/public",
          constraint: %ResourceConstraint{
            resource_types: user_account_resource_types() # TODO: user_account_resource_types don't belong here. Needs data-redistribution over different graphs-work.
            }
          },
        ]
      },

      %GroupSpec{
        name: "o-admin-roles",
        useage: [:read, :write, :read_for_write],
        access: access_by_group( "<http://data.kanselarij.vlaanderen.be/id/group/admin>" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/admin",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"]
            }
          },
        ]
      },
      %GroupSpec{
        name: "o-intern-regering-read",
        useage: [:read, :write, :read_for_write],
        access: access_by_group( "<http://data.kanselarij.vlaanderen.be/id/group/kabinet>" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/intern-regering",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"] ++file_bundling_resource_types()
            }
          },
        ]
      },
      %GroupSpec{
        name: "o-minister-read",
        useage: [:read, :write, :read_for_write],
        access: access_by_group( "<http://data.kanselarij.vlaanderen.be/id/group/minister>" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/minister",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"] ++file_bundling_resource_types()
            }
          },
        ]
      },
      %GroupSpec{
        name: "o-kanselarij-all",
        useage: [:read, :write, :read_for_write],
        access: access_by_group( "<http://data.kanselarij.vlaanderen.be/id/group/admin>
                                 <http://data.kanselarij.vlaanderen.be/id/group/kanselarij>"),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/kanselarij",
            constraint: %ResourceConstraint{
              resource_types: newsletter_resource_types() ++
                agendering_resource_types() ++
                generic_besluitvorming_resource_types() ++
                document_resource_types() ++
                unconfidential_resource_types() ++
                static_unconfidential_code_list_types() ++
                user_account_resource_types() ++
                file_bundling_resource_types() ++
                publication_resource_types() ++
                sign_resource_types() ++
                staatsblad_resource_types()
            }
          },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/email",
            constraint: %ResourceConstraint{
              resource_types: email_resource_types()
            }
          },
        ]
      },
      %GroupSpec{
        name: "ovrb",
        useage: [:read, :write, :read_for_write],
        access: access_by_group( "<http://data.kanselarij.vlaanderen.be/id/group/OVRB>" ), # TODO: Read access on whole "kanselarij"-graph for now. Recent kanselarij-data will have separate graph later on
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/kanselarij",
            constraint: %ResourceConstraint{
              resource_types: generic_besluitvorming_resource_types() ++
                document_resource_types() ++
                publication_resource_types() ++
                sign_resource_types() ++
                staatsblad_resource_types()
            }
          },
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/system/email",
            constraint: %ResourceConstraint{
              resource_types: email_resource_types()
            }
          },
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

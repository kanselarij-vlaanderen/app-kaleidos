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

  defp named_graph_access_by_role( group_string, graph_name ) do
    %AccessByQuery{
      vars: ["name"],
      query: named_sparql_query_for_access_role( group_string, graph_name ) }
  end

  defp named_sparql_query_for_access_role( group_string, graph_name ) do
    "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX session: <http://mu.semte.ch/vocabularies/session/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name ?session_role WHERE {
      BIND(\"#{graph_name}\" AS ?name)
      <SESSION_ID> session:account / ^foaf:account / ^foaf:member ?group .
      BIND( STRAFTER(STR(?group), \"http://data.kanselarij.vlaanderen.be/id/group/\") AS ?session_role )
      FILTER(?session_role IN (\"#{group_string}\") )
    } LIMIT 1"
  end

  defp direct_write_on_public( group_string ) do
    %AccessByQuery{
      vars: [],
      query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      SELECT ?session_role WHERE {
        <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                     ext:sessionRole ?session_role.
        FILTER( ?session_role IN (\"#{group_string}\") )
      } LIMIT 1" }
  end

  defp generic_besluitvorming_resource_types() do
    [
      "https://data.vlaanderen.be/ns/dossier#Dossier",
      "https://data.vlaanderen.be/ns/dossier#Procedurestap",
      "http://www.w3.org/ns/prov#Activity",
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
      "http://mu.semte.ch/vocabularies/ext/publicatie/ContactPersoon",
      "http://mu.semte.ch/vocabularies/ext/publicatie/NumacNummer",
      "http://www.w3.org/ns/org#Organization",
    ]
  end

  defp email_resource_types() do
    [
      # "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#Mailbox",
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#Folder",
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#Email",
      # "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#MessageHeader",
    ]
  end

  defp newsletter_resource_types() do
    [
      "http://data.vlaanderen.be/ns/besluitvorming#NieuwsbriefInfo",
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
      "http://mu.semte.ch/vocabularies/ext/Goedkeuring",
      "http://mu.semte.ch/vocabularies/ext/DocumentIdentifier", # TODO: check if this type is in use.
      "http://data.vlaanderen.be/ns/mandaat#Mandaat",
      "http://data.vlaanderen.be/ns/mandaat#Mandataris",
      "http://www.w3.org/ns/person#Person",
      "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatie",
      "http://mu.semte.ch/vocabularies/ext/Handtekening", # TODO: check if this type is in use.
      "http://mu.semte.ch/vocabularies/ext/MailCampagne", # TODO: check if type is truly unconfidential.
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
      "http://mu.semte.ch/vocabularies/ext/ToegangsniveauCode",
      "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
      "http://mu.semte.ch/vocabularies/ext/DossierTypeCode",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapType",
      "http://kanselarij.vo.data.gift/core/IseCode",
      "http://kanselarij.vo.data.gift/core/Beleidsdomein",
      "http://kanselarij.vo.data.gift/core/Beleidsveld",
      "http://mu.semte.ch/vocabularies/ext/publicatie/Publicatiestatus",
      "http://mu.semte.ch/vocabularies/ext/publicatie/Publicatietype",
      "http://mu.semte.ch/vocabularies/ext/ActiviteitType",
      "http://publications.europa.eu/ontology/euvoc#Language",
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
        graphs: [ %GraphSpec{
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
            } } ]
      },
      %GroupSpec{
        name: "o-intern-overheid-read",
        useage: [:read, :write, :read_for_write],
        access: named_graph_access_by_role( "overheid", "intern-overheid" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"] ++file_bundling_resource_types()
            }
          }
        ]
      },
      %GroupSpec{
        name: "o-admin-on-public",
        useage: [:write, :read_for_write],
        access: direct_write_on_public( "admin" ),
        graphs: [ %GraphSpec{
          graph: "http://mu.semte.ch/graphs/public",
          constraint: %ResourceConstraint{
            resource_types: [
              "http://www.w3.org/ns/person#Person",
              "http://xmlns.com/foaf/0.1/OnlineAccount",
              "http://xmlns.com/foaf/0.1/Person",
              "http://xmlns.com/foaf/0.1/Group",
            ] ++ unconfidential_resource_types() ++
              static_unconfidential_code_list_types() ++
              user_account_resource_types()
            }
          },
        ]
      },
      %GroupSpec{
        name: "o-kanselarij-on-public",
        useage: [:write, :read_for_write],
        access: direct_write_on_public( "kanselarij" ),
        graphs: [ %GraphSpec{
          graph: "http://mu.semte.ch/graphs/public",
          constraint: %ResourceConstraint{
            resource_types: unconfidential_resource_types() ++
              static_unconfidential_code_list_types() ++
              user_account_resource_types() # TODO: user_account_resource_types don't belong here. Needs data-redistribution over different graphs-work.
          } },
        ]
      },

      %GroupSpec{
        name: "o-admin-roles",
        useage: [:read, :write, :read_for_write],
        access: named_graph_access_by_role( "admin", "admin" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"]
            }
          },
        ]
      },
      %GroupSpec{
        name: "o-intern-regering-read",
        useage: [:read, :write, :read_for_write],
        access: named_graph_access_by_role( "kabinet", "intern-regering" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"] ++file_bundling_resource_types()
            }
          },
        ]
      },
      %GroupSpec{
        name: "o-minister-read",
        useage: [:read, :write, :read_for_write],
        access: named_graph_access_by_role( "minister", "minister" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"] ++file_bundling_resource_types()
            }
          },
        ]
      },
      %GroupSpec{
        name: "o-kanselarij-all",
        useage: [:read, :write, :read_for_write],
        access: named_graph_access_by_role( "kanselarij\", \"minister president\", \"admin", "kanselarij" ),
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
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
                email_resource_types()
            }
          },
        ]
      },
      %GroupSpec{
        name: "ovrb",
        useage: [:read, :write, :read_for_write],
        access: named_graph_access_by_role( "OVRB", "kanselarij" ), # TODO: Read access on whole "kanselarij"-graph for now. Recent kanselarij-data will have separate graph later on
        graphs: [
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
            constraint: %ResourceConstraint{
              resource_types: publication_resource_types() ++
                generic_besluitvorming_resource_types() ++
                document_resource_types() ++
                email_resource_types()
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

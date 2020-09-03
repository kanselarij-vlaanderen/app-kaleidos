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

  # TODO get this from the database?
  defp all_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/Goedkeuring",
      "http://data.vlaanderen.be/ns/besluitvorming#Agenda",
      "http://data.vlaanderen.be/ns/besluit#Agendapunt",
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
      "http://xmlns.com/foaf/0.1/Document", # TODO: Delete after complete document migration
      "https://data.vlaanderen.be/ns/dossier#Serie",
      "http://mu.semte.ch/vocabularies/ext/DocumentVersie", # TODO: Delete after complete document migration
      "https://data.vlaanderen.be/ns/dossier#Stuk",
      "https://data.vlaanderen.be/ns/dossier#Dossier",
      "http://dbpedia.org/ontology/UnitOfWork",
      "http://data.vlaanderen.be/ns/besluitvorming#NieuwsbriefInfo",
      "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
      "http://data.vlaanderen.be/ns/besluit#Vergaderactiviteit",
      "http://mu.semte.ch/vocabularies/ext/DocumentIdentifier",
      "http://schema.org/Comment",
      "http://mu.semte.ch/vocabularies/ext/DocumentTypeCode",
      "http://mu.semte.ch/vocabularies/ext/Notule",
      "http://mu.semte.ch/vocabularies/ext/ThemaCode",
      "http://mu.semte.ch/vocabularies/ext/Thema",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatieType",
      "http://mu.semte.ch/vocabularies/ext/BeslissingsResultaatCode",
      "http://mu.semte.ch/vocabularies/ext/ToegangsniveaCode", # TODO: fix missing "u"
      "http://data.vlaanderen.be/ns/mandaat#Mandaat",
      "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
      "http://data.vlaanderen.be/ns/mandaat#Mandataris",
      "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
      "http://mu.semte.ch/vocabularies/ext/DossierTypeCode",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatie",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapType",
      "http://kanselarij.vo.data.gift/core/IseCode",
      "http://kanselarij.vo.data.gift/id/mandatarissen/",
      "http://mu.semte.ch/vocabularies/ext/Handtekening",
      "http://kanselarij.vo.data.gift/core/Beleidsdomein",
      "http://kanselarij.vo.data.gift/core/Beleidsveld",
      "http://www.w3.org/ns/person#Person",
      "http://mu.semte.ch/vocabularies/ext/MailCampagne",
      "http://www.w3.org/ns/org#Organization",
      "http://data.vlaanderen.be/ns/besluitvorming#Agendering",
      "http://www.w3.org/ns/adms#Identifier",
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

  # Also insert your type as ext:PublicClass 
  defp unconfidential_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/Goedkeuring",
      "http://mu.semte.ch/vocabularies/ext/DocumentIdentifier",
      "http://schema.org/Comment",
      "http://mu.semte.ch/vocabularies/ext/DocumentTypeCode",
      "http://mu.semte.ch/vocabularies/ext/ThemaCode",
      "http://mu.semte.ch/vocabularies/ext/Thema",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatieType",
      "http://mu.semte.ch/vocabularies/ext/BeslissingsResultaatCode",
      "http://xmlns.com/foaf/0.1/OnlineAccount",
      "http://xmlns.com/foaf/0.1/Person",
      "http://xmlns.com/foaf/0.1/Group",
      "http://mu.semte.ch/vocabularies/ext/ToegangsniveauCode",
      "http://data.vlaanderen.be/ns/mandaat#Mandaat",
      "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
      "http://data.vlaanderen.be/ns/mandaat#Mandataris",
      "http://www.w3.org/ns/person#Person",
      "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
      "http://mu.semte.ch/vocabularies/ext/DossierTypeCode",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatie",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapType",
      "http://kanselarij.vo.data.gift/core/IseCode",
      "http://kanselarij.vo.data.gift/core/Beleidsdomein",
      "http://kanselarij.vo.data.gift/core/Beleidsveld",
      "http://mu.semte.ch/vocabularies/ext/Handtekening",
      "http://www.w3.org/ns/person#Person",
      "http://mu.semte.ch/vocabularies/ext/MailCampagne",
      "http://www.w3.org/ns/org#Organization",
      "http://www.w3.org/ns/adms#Identifier",
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
            resource_types: unconfidential_resource_types()
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
            ] ++ unconfidential_resource_types() } },
        ]
      },
      %GroupSpec{
        name: "o-kanselarij-on-public",
        useage: [:write, :read_for_write],
        access: direct_write_on_public( "kanselarij" ),
        graphs: [ %GraphSpec{
          graph: "http://mu.semte.ch/graphs/public",
          constraint: %ResourceConstraint{
            resource_types: unconfidential_resource_types()
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
              resource_types: all_resource_types() ++file_bundling_resource_types()
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

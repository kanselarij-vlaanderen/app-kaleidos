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
  defp access_by_role( group_string ) do
    %AccessByQuery{
      vars: ["session_group"],
      query: sparql_query_for_access_role( group_string ) }
  end

  defp sparql_query_for_access_role( group_string ) do
    "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT ?session_group ?session_role WHERE {
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                   ext:sessionRole ?session_role.
      FILTER( ?session_role = \"#{group_string}\" )
    } LIMIT 1"
  end

  defp named_graph_access_by_role( group_string, graph_name ) do
    %AccessByQuery{
      vars: ["name"],
      query: named_sparql_query_for_access_role( group_string, graph_name ) }
  end

  defp named_sparql_query_for_access_role( group_string, graph_name ) do
    "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT ?name ?session_role WHERE {
      BIND(\"#{graph_name}\" AS ?name)
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                   ext:sessionRole ?session_role.
      FILTER( ?session_role IN (\"#{group_string}\") )
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
      "http://xmlns.com/foaf/0.1/Document",
      "http://mu.semte.ch/vocabularies/ext/DocumentVersie",
      "http://dbpedia.org/ontology/Case",
      "http://dbpedia.org/ontology/UnitOfWork",
      "http://data.vlaanderen.be/ns/besluitvorming#NieuwsbriefInfo",
      "http://data.vlaanderen.be/ns/besluit#Besluit",
      "http://data.vlaanderen.be/ns/besluit#Zitting",
      "http://mu.semte.ch/vocabularies/ext/DocumentIdentifier",
      "http://schema.org/Comment",
      "http://mu.semte.ch/vocabularies/ext/DocumentTypeCode",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapFase",
      "http://mu.semte.ch/vocabularies/ext/Notule",
      "http://mu.semte.ch/vocabularies/ext/ThemaCode",
      "http://mu.semte.ch/vocabularies/ext/Thema",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatieType",
      "https://data.vlaanderen.be/ns/besluitvorming#Mededeling",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapFaseCode",
      "http://mu.semte.ch/vocabularies/ext/VertrouwelijkheidCode",
      "http://data.vlaanderen.be/ns/mandaat#Mandaat",
      "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
      "http://data.vlaanderen.be/ns/mandaat#Mandataris",
      "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
      "http://mu.semte.ch/vocabularies/ext/DossierTypeCode",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatie",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapType",
      "http://kanselarij.vo.data.gift/core/IseCode",
      "http://kanselarij.vo.data.gift/id/concept/policy-level/",
      "http://kanselarij.vo.data.gift/id/concept/submitter/",
      "http://kanselarij.vo.data.gift/id/mandatarissen/",
      "http://mu.semte.ch/vocabularies/ext/Handtekening",
      "http://data.vlaanderen.be/ns/besluitvorming#Verdaagd",
      "http://mu.semte.ch/vocabularies/ext/oc/Meeting",
      "http://mu.semte.ch/vocabularies/ext/oc/AgendaItem",
      "http://mu.semte.ch/vocabularies/ext/oc/Case",
      "http://kanselarij.vo.data.gift/core/Beleidsdomein",
      "http://kanselarij.vo.data.gift/core/Beleidsveld",
      "http://www.w3.org/ns/person#Person",
      "http://mu.semte.ch/vocabularies/ext/MailCampagne"
    ]
  end

  defp unconfidential_resource_types() do
    [
      "http://mu.semte.ch/vocabularies/ext/Goedkeuring",
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
      "http://data.vlaanderen.be/ns/besluitvorming#NieuwsbriefInfo",
      "http://mu.semte.ch/vocabularies/ext/DocumentIdentifier",
      "http://schema.org/Comment",
      "http://mu.semte.ch/vocabularies/ext/DocumentTypeCode",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapFase",
      "http://mu.semte.ch/vocabularies/ext/Notule",
      "http://mu.semte.ch/vocabularies/ext/ThemaCode",
      "http://mu.semte.ch/vocabularies/ext/Thema",
      "http://mu.semte.ch/vocabularies/ext/SysteemNotificatieType",
      "http://xmlns.com/foaf/0.1/OnlineAccount",
      "http://xmlns.com/foaf/0.1/Person",
      "http://xmlns.com/foaf/0.1/Group",
      "https://data.vlaanderen.be/ns/besluitvorming#Mededeling",
      "http://mu.semte.ch/vocabularies/ext/ProcedurestapFaseCode",
      "http://mu.semte.ch/vocabularies/ext/VertrouwelijkheidCode",
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
      "http://data.vlaanderen.be/ns/besluitvorming#Verdaagd",
      "http://www.w3.org/ns/person#Person",
      "http://mu.semte.ch/vocabularies/ext/MailCampagne"
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
        useage: [:read],
        access: named_graph_access_by_role( "privileged", "intern-overheid" ),
        graphs: [ 
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
            constraint: %ResourceConstraint{
              resource_types: ["http://mu.semte.ch/vocabularies/ext/NotAThing"]
            } 
          } 
        ]
      },
      %GroupSpec{
        name: "o-admin-on-public",
        useage: [:read, :write, :read_for_write],
        access: direct_write_on_public( "admin" ),
        graphs: [ %GraphSpec{
          graph: "http://mu.semte.ch/graphs/public",
          constraint: %ResourceConstraint{
            resource_types: [
              "http://www.w3.org/ns/person#Person",
              "http://xmlns.com/foaf/0.1/OnlineAccount",
              "http://xmlns.com/foaf/0.1/Person",
              "http://xmlns.com/foaf/0.1/Group",
            ] } },
        ]
      },
      %GroupSpec{
        name: "o-kanselarij-on-public",
        useage: [:read, :write, :read_for_write],
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
        graphs: [ %GraphSpec{
          graph: "http://mu.semte.ch/graphs/organizations/",
          constraint: %ResourceConstraint{
            resource_types: [
              "http://mu.semte.ch/vocabularies/ext/NotAThing",                                                 
            ] } },
        ] 
      },
      %GroupSpec{
        name: "o-intern-regering-read",
        useage: [:read],
        access: named_graph_access_by_role( "kabinet\", \"minister", "intern-regering" ),
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
        name: "o-kanselarij-all",
        useage: [:read, :write, :read_for_write],
        access: named_graph_access_by_role( "kanselarij\", \"minister president\", \"admin", "kanselarij" ),
        graphs: [ 
          %GraphSpec{
            graph: "http://mu.semte.ch/graphs/organizations/",
            constraint: %ResourceConstraint{
              resource_types: all_resource_types()
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

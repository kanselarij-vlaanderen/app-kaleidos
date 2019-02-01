require_relative '/usr/src/app/sinatra_template/utils.rb'

FOAF =  RDF::Vocabulary.new('http://xmlns.com/foaf/0.1/')

module LoginConfig
  extend self

  def group_filter
      filter = "GRAPH <#{graph}> {"
      filter += "  ?group a <#{FOAF.Group}> ;"
      filter += "           <#{MU_CORE.uuid}> ?group_uuid ."
      filter += "}"
  end

  def group_type_name
    "account-groups"
  end
  
end
    

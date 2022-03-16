# Changes to Link prediction method on global + new functions...
# for both descriptive statistics and analysis


# Predict Links on all graph

plot.predicted.links <- function(g.trump, weight_input) {
  m.predicted.edges <-
    as.matrix(cocitation(g.trump) * (1-get.adjacency(g.trump)))
  g.predicted.edges <-
    graph_from_adjacency_matrix(m.predicted.edges,
                                mode = "undirected",
                                weighted = TRUE)
  E(g.predicted.edges)$width <- E(g.predicted.edges)$weight * 2
  edges.to.keep <- E(g.predicted.edges)[which(E(g.predicted.edges)$weight == weight_input)] 
  g.weighted.edges <- subgraph.edges(g.predicted.edges, edges.to.keep, delete.vertices = TRUE)
  plot(g.weighted.edges, vertex.label = ifelse(degree(g.weighted.edges) > 4, V(g.weighted.edges)$name, NA))
  return(g.weighted.edges)
}

# Predict links on specific graph (person-person, organization-organization, person-organization)
plot.predicted.links <- function(g.trump, weight_input, entity_type_1, entity_type_2) {
  g.entity.type <- entity.type.plot(g.trump, entity_type_1, entity_type_2)
  m.predicted.edges <-
    as.matrix(cocitation(g.entity.type) * (1-get.adjacency(g.entity.type)))
  g.predicted.edges <-
    graph_from_adjacency_matrix(m.predicted.edges,
                                mode = "undirected",
                                weighted = TRUE)
  E(g.predicted.edges)$width <- E(g.predicted.edges)$weight * 2
  edges.to.keep <- E(g.predicted.edges)[which(E(g.predicted.edges)$weight == weight_input)] 
  g.weighted.edges <- subgraph.edges(g.predicted.edges, edges.to.keep, delete.vertices = TRUE)
  plot(g.weighted.edges, vertex.label = ifelse(degree(g.weighted.edges) > 4, V(g.weighted.edges)$name, NA))
  return(g.weighted.edges)
}


#Test
plot.predicted.links(g.trump, 2, "Organization", "Organization")

#Digging deeper
# Look at ownership and investor

# Plots entity_type subgraph based on connection type indicated, only plots label for degrees > N
top.entity.connection.types.plot <- function (input, n_degree, entity_type) {
  vertices.to.delete <- V(g.trump)[which(V(g.trump)$Entity_Type != entity_type)]
  subgraph.gtrump <- delete.vertices(g.trump, vertices.to.delete)
  edges.to.keep <- E(subgraph.gtrump)[which(E(subgraph.gtrump)$Connection == input)]
  g.trump.filtered <- subgraph.edges(subgraph.gtrump, eids = edges.to.keep, delete.vertices = TRUE)
  plot(g.trump.filtered, vertex.size = 1, vertex.label = ifelse(degree(g.trump.filtered) > n_degree, V(g.trump.filtered)$name, NA))
  return(g.trump.filtered)
}
g.ownership.organization <- top.organization.connection.types.plot("Ownership", 5, "Organization")

# In g.ownership.organization, we have a graph with all the organizations that own other organizations

explore.subgraph <- function(input, n_degree, entity_type1, entity_type_2) {
  g.entity.type <- entity.type.plot(g.trump, entity_type1, entity_type_2)  #Already only entities interested
  edges.to.keep <- E(g.entity.type)[which(E(g.entity.type)$Connection == input)]
  g.connection.entity <- subgraph.edges(g.entity.type, eids = edges.to.keep, delete.vertices = TRUE)
  which.max(degree(g.connection.entity))
  g.connection.entity.decomposed <- decompose(g.connection.entity)
  largest <- which.max(sapply(g.connection.entity.decomposed, diameter))
  plot(g.connection.entity.decomposed[[largest]],
       layout=layout.fruchterman.reingold,
       vertex.label.cex = .5,
       vertex.size = 5,
       edge.arrow.size = .1
  )
}
explore.subgraph("Investor", 1, "Organization", "Person")

# What are the top connections BY entity - entity ? So person-person, org-org and person-org?

#Create top N by organization type - Basic statistics

top.entities.type <- function(entity_type_category_A, entity_type_category_B, top_number) {
  unique(dt.trump[Entity_A_Type == entity_type_category_A][Entity_B_Type == entity_type_category_B][, count_connections := .N, by = "Connection"]
         [order(-count_connections)], by = "Connection")[1:top_number][, !c("Entity_A", "Entity_B", "Sources")]
}
top.entities.type("Organization", "Organization", 10)
library(data.table)
library(igraph)

# Load data
dt.trump <- fread("TrumpWorld-Data.csv")

# Change column names 
colnames(dt.trump) <- c("Entity_A_Type", "Entity_A", "Entity_B_Type", "Entity_B", "Connection", "Sources")

# Retrieve vertices
all.entity.A <- dt.trump[, list(name=unique(Entity_A))]
all.entity.B <- dt.trump[, list(name=unique(Entity_B))]
all.entities <- rbind(all.entity.A, all.entity.B)
unique.entities <- unique(all.entities)

# Retrieve vertices attributes (type)
dt.trump.entityA.attributes <- dt.trump[Entity_A %in% unique.entities$name][, c("Entity_A", "Entity_A_Type")]
dt.trump.entityB.attributes <- dt.trump[Entity_B %in% unique.entities$name][, c("Entity_B", "Entity_B_Type")]
dt.all.entities.attributes <- rbind(dt.trump.entityA.attributes, dt.trump.entityB.attributes, use.names = FALSE)
dt.unique.entities.attributes <- unique(dt.all.entities.attributes)

# Change Names of Columns
colnames(dt.unique.entities.attributes) <- c("Name", "Entity_Type")
colnames(dt.all.entities.attributes) <- c("Name", "Entity_Type")

#Relationship Table with Connection as Edge attribute
dt.trump.connections <- dt.trump[, c("Entity_A", "Entity_B", "Connection")]

# Build undirected graph
g.trump <- graph.data.frame(dt.trump.connections, directed = FALSE, vertices = dt.unique.entities.attributes)
plot(g.trump, vertex.size = 0.05, vertex.label = NA)

# Count vertices
V(g.trump)$Entity_Type == "Organization"
  
summary(g.trump)

# Basic descriptive statistics
total.entities.type <- function(entity_type_category) {
  dt.unique.entities.attributes[Entity_Type == entity_type_category, .N]
}
total.entities.person <- total.entities.type("Person")
total.entities.person <- total.entities.type("Organization")
total.entities.person <- total.entities.type("Federal Agency")

total.entities.type("Organization")

top.entities.name <- function(top_number) {
  unique(dt.all.entities.attributes[, entity_name_count := .N, by=Name][order(-entity_name_count)])[1:top_number]
}

#Create top N by organization type

top.entities.type <- function(entity_type_category, top_number) {
  unique(dt.all.entities.attributes[, entity_name_count := .N, by=Name][Entity_Type == entity_type_category][order(-entity_name_count)])[1:top_number]
}

top.entities.type("Organization", 10)


# Create function to build subgraphs

entity.type.plot <- function(g.trump, entity_type_1, entity_type_2) {
  entity_connection <- paste(entity_type_1, entity_type_2, sep="")
  el <- get.edgelist(g.trump)
  E(g.trump)$entity_type_connection <- paste(V(g.trump)[el[, 1]]$Entity_Type, V(g.trump)[el[, 2]]$Entity_Type, sep = "")
  if (entity_connection == "PersonPerson") {
    edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "PersonPerson")]
  } else if (entity_connection == "OrganizationOrganization") {
    edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "OrganizationOrganization")]
  } else {edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == c("OrganizationPerson", "PersonOrganization"))]
  } 
  g.trump.filtered <- subgraph.edges(g.trump, eids = edges.to.keep, delete.vertices = TRUE)
  plot(g.trump.filtered)
  V(g.trump.filtered)
  return(g.trump.filtered)
}


g.person.person <- entity.type.plot(g.trump, "Person", "Person")


# Create function that calculates centrality measure (locally) and retrieves top N, highlights them in network

entity.top.centrality.plot <- function(measure, top_number)  {
  dt.centrality <- data.table(Node=V(g.trump)$name,
             Degree=degree(g.trump),
             Closeness=round(closeness(g.trump), 2),
             Betweenness=betweenness(g.trump),
             Eigenvector=round(evcent(g.trump)$vector, 2))
  if (measure %in% c("Betweenness", "betweenness")){
    data.sorted <- dt.centrality[order(dt.centrality$Betweenness, decreasing = TRUE), ]
    top.nodes <- head(data.sorted, top_number)
  } else if (measure %in% c("degree", "Degree"))  {
    data.sorted <- dt.centrality[order(dt.centrality$Degree, decreasing = TRUE), ] 
    top.nodes <- head(data.sorted, top_number)
  } else if (measure %in% c("closeness", "Closeness")) {
    data.sorted <- dt.centrality[order(dt.centrality$Closeness, decreasing = TRUE), ]
    top.nodes <- head(data.sorted, top_number)
  }  else {data.sorted <- dt.centrality[order(dt.centrality$Eigenvector, decreasing = TRUE), ]
  top.nodes <- head(data.sorted, top_number)
  }
  V(g.trump)$color <- ifelse(V(g.trump)$name %in% top.nodes[,1], "red", "blue")
  plot(g.trump, vertex.label = NA, vertex.size = 0.5)
}

# Top 10 connection types

# Basic statistics
top.10.connections <- unique(dt.trump.connections[, count_connections := .N, by = "Connection"], by = "Connection")[order(-count_connections)][1:10][, !c("Entity_A", "Entity_B")]
top.10.connections


# Plots subgraph based on connection type indicated, only plots label for degrees > N
top.connection.types.plot <- function (input, n_degree) {
  edges.to.keep <- E(g.trump)[which(E(g.trump)$Connection == input)]
  g.trump.filtered <- subgraph.edges(g.trump, eids = edges.to.keep, delete.vertices = TRUE)
  plot(g.trump.filtered, vertex.size = 1, vertex.label = ifelse(degree(g.trump.filtered) > n_degree, V(g.trump.filtered)$name, NA))
}

# Test
top.connection.types.plot("Director", 5)


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


# TO DO: Analyze results of previous functions created
# Fix the link prediction method (either create a subgraph for predictions or analyze based on weights)


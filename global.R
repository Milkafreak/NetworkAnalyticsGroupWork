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
colnames(dt.all.entities.attributes) <- c("Name", "Entity_Type", "Entity_Count")

#Relationship Table with Connection as Edge attribute
dt.trump.connections <- dt.trump[, c("Entity_A", "Entity_B", "Connection")]

# Build undirected graph
g.trump <- graph.data.frame(dt.trump.connections, directed = FALSE, vertices = dt.unique.entities.attributes)
plot(g.trump, vertex.size = 0.05, vertex.label = NA)

# Basic descriptive statistics
total.entities.type <- function(entity_type_category) {
  dt.unique.entities.attributes[Entity_Type == entity_type_category, .N]
}
total.entities.person <- total.entities.type("Person")
total.entities.person <- total.entities.type("Organization")
total.entities.person <- total.entities.type("Federal Agency")

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
}


entity.type.plot(g.trump, "Person", "Person")


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

top.connection.types.plot <- function (top_number, input) {
  
  
}
  
  
#Calculate top 10 connection type
#User selects 
#Create subgraph
entity.top.centrality.plot("degree", 50)

summary(g.trump)


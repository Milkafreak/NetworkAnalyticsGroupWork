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


# Graph with connection type input

connection.type.plot <- function(g.trump, connection) {
  
  edges.to.keep <- E(g.trump)[which(E(g.trump)$Connection == connection)]
  g.trump.filtered <- subgraph.edges(g.trump, eids = edges.to.keep, delete.vertices = TRUE)
  V(g.trump.filtered)$label <- ''
  V(g.trump.filtered)$size <- 3
  plot(g.trump.filtered)
}

# Centrality Measures - function that calculates centrality measure (locally) and retrieves top n

entity.top.centrality <- function(centrality_measure, top_number)  {
  
  dt.centrality <- data.table(Node=V(g.trump)$name,
                              Degree=degree(g.trump),
                              Closeness=round(closeness(g.trump), 2),
                              Betweenness=betweenness(g.trump),
                              Eigenvector=round(evcent(g.trump)$vector, 2))
  
  dt.centrality[order((centrality_measure), decreasing = TRUE)]
  print(dt.centrality)
}


entity.top.centrality("Betweenness", 10)


# Neighbors

neighbors <- function(entity) {
  
  g.neighbors.entity <- neighbors(g.trump, V(g.trump)$name == name)
  
  g.neighbors <- induced.subgraph(g.trump, vids = (V(g.trump) %in% g.neighbors.entity) | (V(g.trump)$name == name))
  
  V(g.neighbors)$label <- ''
  plot(g.neighbors, vertex.label.cex=0.8, vertex.label.dist=4)
  
}

neighbors('ALABAMA POLICY INSTITUTE')
library(data.table)
library(igraph)
library(ggplot2)
library(shiny)

# Load data
dt.trump <- fread("TrumpWorld-Data.csv")


# Change column names 
colnames(dt.trump) <- c("Entity_A_Type", "Entity_A", "Entity_B_Type", "Entity_B", "Connection", "Sources")


dt.trump <- dt.trump %>% mutate(entity_type_connection = paste(Entity_A_Type,Entity_B_Type))


# Retrieve vertices
all.entity.A <- dt.trump[, list(name=unique(Entity_A))]
all.entity.B <- dt.trump[, list(name=unique(Entity_B))]
all.entities <- rbind(all.entity.A, all.entity.B)
unique.entities <- unique(all.entities)

#Relationship Table with Connection as Edge attribute
dt.trump.connections <- dt.trump[, c("Entity_A", "Entity_B", "Connection","entity_type_connection")]

# Retrieve vertices attributes (type)
dt.trump.entityA.attributes <- dt.trump[Entity_A %in% unique.entities$name][, c("Entity_A", "Entity_A_Type")]
dt.trump.entityB.attributes <- dt.trump[Entity_B %in% unique.entities$name][, c("Entity_B", "Entity_B_Type")]
dt.all.entities.attributes <- rbind(dt.trump.entityA.attributes, dt.trump.entityB.attributes, use.names = FALSE)
dt.unique.entities.attributes <- unique(dt.all.entities.attributes)

# Create undirected graph
g.trump <- graph.data.frame(dt.trump.connections, directed = FALSE, vertices = dt.unique.entities.attributes)
V(g.trump)$entity_type <- dt.unique.entities.attributes$Entity_A_Type
g.tidy <- as_tbl_graph(g.trump) 


# Create datatable with number of observations
by_type <- dt.unique.entities.attributes %>% count(Entity_A_Type)
dt.by_type <- data.table(by_type)
colnames(dt.by_type) <- c("Entity_Type", "Number_of_Observations")

# Create bar chart for previous data table
bar.plot.entities <- ggplot(dt.by_type, aes(x=Entity_Type, y=Number_of_Observations)) + 
  geom_bar(stat = "identity")


# Create datatable with number of connection
count.connections <- dt.trump.connections %>% count(Connection)
count.connections.order <- count.connections[order(-count.connections$n,),]
dt.count.connections.order <- data.table(count.connections.order)
colnames(dt.count.connections.order) <- c("Connection_Type", "Number_of_Observations")
dt.top.connections <- head(dt.count.connections.order[order(-rank(Number_of_Observations))], 6)

# Create bar chart for previous data table
bar.plot.connections <- ggplot(dt.top.connections, aes(x=Connection_Type, y=Number_of_Observations)) + 
  geom_bar(stat = "identity")


# Create histogram with degree distribution
hist_degree <- degree(g.trump)
degree.histogram <- as.data.frame(table(hist_degree))
degree.histogram[,1] <- as.numeric( paste(degree.histogram[,1]))
degree_hist <- ggplot(data=degree.histogram, aes(x=hist.degree, y=Freq)) +  geom_bar(stat="identity") + coord_flip() + scale_y_log10()


# Create dataframe with only "Person","Organization" and "Federal Agency"
df <- data.frame(Type = c("Person","Organization","Federal Agency"),
                 Appear = c(TRUE,TRUE,TRUE))

#load("nda-similarity.RData")
load("trump.RData")

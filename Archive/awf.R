#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('rsconnect')
#install.packages("shiny")

library(igraph)
library(shiny)
library(data.table)
library(rsconnect)
library(dplyr)
library(tidygraph)


dt.trump <- fread("TrumpWorld-Data.csv")


rsconnect::setAccountInfo(name='projecttrumpteam13',
                          token='1DE3AD67152B76A1058205816E3394A0',
                          secret='7JSX5gNCUhgQv7GFvDKy/vGI1UTgWZ9rhJ8/n15T')




#SERVER
library(shiny)
library(ggplot2)

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

dt.trump.connections <- dt.trump[, c("Entity_A", "Entity_B", "Connection")]
g.trump <- graph.data.frame(dt.trump.connections, directed = FALSE, vertices = dt.unique.entities.attributes)


View(dt.trump)

View(dt.trump.connections)


V(g.trump)
V(g.trump)$entity_type = dt.unique.entities.attributes$Entity_A_Type

length(V(g.trump))
length(dt.all.entities.attributes$Entity_A_Type)


V(g.trump)$entity_type

g.tidy <- as_tbl_graph(g.trump) 

g.tidy %>% activate(nodes) %>% filter(entity_type == "Person")
g.tidy %>% activate(nodes) %>% filter(entity_type == "Organization")
g.tidy %>% activate(nodes) %>% filter(entity_type == "Federal Agency")

filtered_types = c("People"=TRUE,"Organization"=TRUE,"Federal Agency"=TRUE)

liste = list(TRUE,TRUE,TRUE)
names(liste) <- c("People","Organization","Federal Agency")
liste
print(liste)

liste == TRUE
liste[1]

liste[1] = FALSE
liste
liste == TRUE
liste.find(TRUE)

liste %>>% list.find(TRUE)
library(rlist)
install.packages("rlist")

c("")

df <- data.frame(Type = c("Person","Organization","Federal Agency"),
                  Appear = c(TRUE,TRUE,TRUE))

c(df[df["Appear"] == TRUE,]$Type)

df[df["Type"] == "Person",] = FALSE

df

g.tidy.x <- g.tidy

g.tidy.x <- g.tidy %>% activate(nodes) %>% filter(entity_type == c(df[df["Appear"] == TRUE,]$Type))
plot(g.tidy.x,vertex.size = 0.05, vertex.label = NA)

V(g.tidy.x)$entity_type
V(g.tidy)$entity_type

c(df[df["Appear"] == TRUE,]$Type)
df

as_tibble(g.tidy.x)

g.tidy %>% activate(nodes) %>% filter(Entity_A_Type %in% c("Organization","Person"))

dt.unique.entities.attributes[dt.unique.entities.attributes$Entity_A_Type == "Organization"]


g.tidy.x <- g.tidy %>% activate(nodes) %>% filter(entity_type %in% c(df[df["Appear"] == TRUE,]$Type))
transitivity(g.tidy.x)
df

transitivity(g.tidy)





Entity_Name = c(V(g.tidy.x)$name[degree(g.tidy.x) %in% liste1[order(-liste1)][1:5]])
Degree = liste1[order(-liste1)][1:5]

Entity_Name



liste1 <- c(as.numeric(names(table(degree(g.tidy)))))
V(g.tidy)$name[degree(g.tidy) %in% liste1[order(-liste1)][1:5]]




data.frame(Entity_Name = names(degree(g.tidy)),
           Degree = degree(g.tidy))

df.degree <- t(data.frame(as.list(degree(g.tidy)),check.names = FALSE,stringsAsFactors = FALSE))

df.betweenness <- t(data.frame(as.list(betweenness(g.tidy)),check.names = FALSE,stringsAsFactors = FALSE))

df.closeness <- t(data.frame(as.list(closeness(g.tidy)),check.names = FALSE,stringsAsFactors = FALSE))

df.eigen <- t(data.frame(as.list(eigen_centrality(g.tidy)),check.names = FALSE,stringsAsFactors = FALSE))

data.frame(Degree=degree(g.tidy),
           Closeness=round(closeness(g.tidy), 2),
           Betweenness=betweenness(g.tidy),
           Eigenvector=round(evcent(g.tidy)$vector, 2)) 

hist(degree_distribution(g.tidy))


hist.degree <- degree(g.trump)
G.degree.histogram <- as.data.frame(table(hist.degree))
G.degree.histogram[,1] <- as.numeric( paste(G.degree.histogram[,1]))
Degree_Hist <- ggplot(data=G.degree.histogram, aes(x=hist.degree, y=Freq)) +  geom_bar(stat="identity") + coord_flip() + ylim(0,100) + xlim(0,100)
plot(Degree_Hist)          

G.degree.histogram
G.degree.histogram

G.degree.histogram <- G.degree.histogram[1:37,]


#Create graph for Louvain
graph <- graph_from_data_frame(edges, directed = FALSE)

#Louvain Comunity Detection
cluster <- cluster_louvain(g.tidy)

cluster

cluster_df <- data.frame(as.list(membership(cluster)),check.names = FALSE,stringsAsFactors = FALSE)
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

V(g.tidy)$cluster <- cluster_df

cluster_df[,1]
#Create group column

V(g.tidy)$cluster

nodes <- left_join(V(g.tidy), cluster_df)
colnames(nodes)[3] <- "group"

vertex_attr(g.tidy, "cluster")

brewer.pal()

plot(g.tidy, vertex.color = brewer.pal(as.numeric(as.factor(vertex_attr(g.tidy, "cluster"))),name="Paired"),vertex.size = 1,vertex.label=NA,)

entity <- "ALABAMA POLICY INSTITUTE"
g.neighbors.entity <- neighbors(g.trump, V(g.trump)$name == entity)
g.neighbors <- induced.subgraph(g.trump, vids = (V(g.trump)%in% g.neighbors.entity) | (V(g.trump)$name == entity))
V(g.neighbors)$label <- ''
plot(g.neighbors, vertex.label.cex=0.8, vertex.label.dist=4)

dt.unique.entities.attributes

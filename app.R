
# Load R packages
library(shiny)
library(igraph)
library(data.table)
library(rsconnect)
library(dplyr)
library(shinythemes)
library(tidygraph)
library(DT)
library(bslib)
library(ggplot2)


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

g.trump <- graph.data.frame(dt.trump.connections, directed = FALSE, vertices = dt.unique.entities.attributes)
V(g.trump)$entity_type = dt.unique.entities.attributes$Entity_A_Type
g.tidy <- as_tbl_graph(g.trump) 

by_type <- dt.unique.entities.attributes %>% count(Entity_A_Type)
dt.by_type <- data.table(by_type)

count.connections <- dt.trump.connections %>% count(Connection)
count.connections.order <- count.connections[order(-count.connections$n,),]
dt.count.connections.order <- data.table(count.connections.order)

hist_degree <- degree(g.trump)
G.degree.histogram <- as.data.frame(table(hist_degree))
G.degree.histogram[,1] <- as.numeric( paste(G.degree.histogram[,1]))
Degree_Hist <- ggplot(data=G.degree.histogram, aes(x=hist.degree, y=Freq)) +  geom_bar(stat="identity") + coord_flip() + scale_y_log10()




df <- data.frame(Type = c("Person","Organization","Federal Agency"),
                 Appear = c(TRUE,TRUE,TRUE))

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("TrumpWorld Data",
                 #theme = bs_theme(bg = "white",
                 #                fg = "midnightblue",
                 #               primary = "maroon",
                 #             base_font = font_google("Montserrat")),
                 
###########################################     PANEL 1  ##########################################################################
                  tabPanel("Descriptive Statistics", icon = icon("bar-chart-o"),
                           sidebarPanel(
                             tags$h4("Adjust the slider to explore the histogram"),
                            
                             sliderInput("XLIM", "Histogram Y-Axis Limit:",
                                         min = 0, max = 757, value = 500
                             ),
                           ), # sidebarPanel
                           mainPanel(
                            # mainPanel
                           dataTableOutput("table.entity_type"),
                           dataTableOutput("table.connection_type"),
                           h4("Degree Distribution Histogram"),
                           plotOutput("hist_degree")
                           )
                           
                  ), # Navbar 1, tabPanel
###########################################     PANEL 2  ##########################################################################
                  tabPanel('Network Exploration',icon = icon("link", lib = "font-awesome"),
                           sidebarPanel(
                             tags$h4("Choose the entity types you want to observe in the network"),
                             checkboxInput("check_Person", "Person", TRUE),
                             checkboxInput("check_Organization", "Organization", TRUE),
                             checkboxInput("check_Federal_Agency", "Federal Agency", TRUE)
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("Network Centrality"),
                             
                             verbatimTextOutput("transitivity"),
                             
                             dataTableOutput("table"),
                             
                             h4("Network Visualization"),
                             verbatimTextOutput("txtout"),
                             plotOutput(outputId = "plot")
                             
                             
                           )),
###########################################     PANEL 3  ##########################################################################
                  tabPanel("Network Analysis", icon = icon("chart-line", lib = "font-awesome"),
                           sidebarPanel(
                             selectInput("entity", 
                                         label = "Choose an entity",unique.entities,
                                         selected = "ALABAMA POLICY INSTITUTE"),
                           
                           ),
                           mainPanel(
                             verbatimTextOutput("txtOut"),
                             
                             plotOutput(outputId = "neighbor_plot"),
                           )
                           
                           ),

###########################################     PANEL 4  ##########################################################################
                 tabPanel("New Panel", icon = icon("chart-line", lib = "font-awesome"),
                          sidebarPanel(
                            selectInput("entity.a",
                                        label = "Choose an entity type for type A",c("Person","Organization"),
                                        selected = "Person")
                            ,
                            selectInput("entity.b",
                                        label = "Choose an entity type for type B",c("Person","Organization"),
                                        selected = "Person"),
                            selectInput("weight",
                                        label = "Choose an assigned weight",c(1,2,3,4),
                                        selected = "1"),
                            selectInput("n_degree",
                                        label = "Choose a n_degree",c(1,2,3,4),
                                        selected = "1"),
                            selectInput("connection_type",
                                        label = "Choose connection type",dt.trump.connections$Connection,
                                        selected = "Investor"),
                            selectInput("EntityType",
                                        label = "Choose an entity type",dt.by_type$Entity_A_Type,
                                        selected = "Organization"),
                            sliderInput("top_number", "Top X Connections between given entity types",
                                        min = 0, max = 25, value = 5
                            ),
                            

                          ),
                          mainPanel(
                            #verbatimTextOutput("txtOut"),

                            plotOutput(outputId = "predicted.links"),
                            plotOutput(outputId = "top.entity.connection.types.plot.plot"),
                            plotOutput(outputId = "explore.subgraph"),
                            textOutput("topConnectionTypes")
                          )

                 ),
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
#######################################################################   REACTIVES   ##########################################################################  
  g.tidy.x <- reactive({
    df[df["Type"] == "Person",]$Appear = input$check_Person
    df[df["Type"] == "Organization",]$Appear = input$check_Organization
    df[df["Type"] == "Federal Agency",]$Appear = input$check_Federal_Agency
    g.tidy.x <- g.tidy %>% activate(nodes) %>% filter(entity_type %in% c(df[df["Appear"] == TRUE,]$Type))
    g.tidy.x
  })
  
  entity.type.plot <- reactive({
    entity_connection <- paste(input$entity.a, input$entity.b, sep=" ")
    if (entity_connection == "Person Person") {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "Person Person")]
    } else if (entity_connection == "Organization Organization") {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "Organization Organization")]
    } else {edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == c("Organization Person", "Person Organization"))]
    } 
    g.trump.filtered <- subgraph.edges(g.trump, eids = edges.to.keep, delete.vertices = TRUE)
    g.trump.filtered
  })
  
  top.entities.type.text <- reactive({
    df.top321 <- unique(dt.trump[Entity_A_Type == input$entity.a][Entity_B_Type == input$entity.b][, count_connections := .N, by = "Connection"]
           [order(-count_connections)], by = "Connection")[1:input$top_number][, !c("Entity_A", "Entity_B", "Sources")]
    df.top123 <- data.frame(df.top321)[c("Connection","count_connections")]
    return(df.top123)
  })
  
  plot.predicted.links1 <- reactive({
    
    g.entity.type <- entity.type.plot() #entity.type.plot()
    m.predicted.edges <-
      as.matrix(cocitation(g.entity.type) * (1-get.adjacency(g.entity.type)))
    g.predicted.edges <-
      graph_from_adjacency_matrix(m.predicted.edges,
                                  mode = "undirected",
                                  weighted = TRUE)
    E(g.predicted.edges)$width <- E(g.predicted.edges)$weight * 2
    edges.to.keep <- E(g.predicted.edges)[which(E(g.predicted.edges)$weight == input$weight)]
    g.weighted.edges <- subgraph.edges(g.predicted.edges, edges.to.keep, delete.vertices = TRUE)
    g.weighted.edges <- plot(g.weighted.edges,vertex.size = 5, vertex.label = ifelse(degree(g.weighted.edges) > 4, V(g.weighted.edges)$name, NA))
    g.weighted.edges
  })
  
  top.entity.connection.types.plot <- reactive ({
    vertices.to.delete <- V(g.trump)[which(V(g.trump)$Entity_Type != input$EntityType)]
    subgraph.gtrump <- delete.vertices(g.trump, vertices.to.delete)
    edges.to.keep <- E(subgraph.gtrump)[which(E(subgraph.gtrump)$Connection == input$connection_type)]
    g.trump.filtered <- subgraph.edges(subgraph.gtrump, eids = edges.to.keep, delete.vertices = TRUE)
    plot(g.trump.filtered, vertex.size = 0.05, vertex.label = ifelse(degree(g.trump.filtered) > input$n_degree, V(g.trump.filtered)$name, NA))
    g.trump.filtered
  })
  
  explore.subgraph <- reactive ({
    g.entity.type1 <- entity.type.plot()  #Already only entities interested
    edges.to.keep <- E(g.entity.type1)[which(E(g.entity.type1)$Connection == input$connection_type)]
    g.connection.entity <- subgraph.edges(g.entity.type1, eids = edges.to.keep, delete.vertices = TRUE)
    which.max(degree(g.connection.entity))
    g.connection.entity.decomposed <- decompose(g.connection.entity)
    largest <- which.max(sapply(g.connection.entity.decomposed, diameter))
    print(largest)
    g.connection <- g.connection.entity.decomposed[[largest]]
    g.connection
  })

#######################################################################   FUNCTIONS   ##########################################################################  
    
  neighbors_plotting <- function() {
    entity <- input$entity
    g.neighbors.entity <- neighbors(g.trump, V(g.trump)$name == entity)
    g.neighbors <- induced.subgraph(g.trump, vids = (V(g.trump)%in% g.neighbors.entity) | (V(g.trump)$name == entity))
    #V(g.neighbors)$label <- ''
    g.neighbors
  }
  
#######################################################################   OUTPUTS   ##########################################################################  
  
  output$txtout <- renderText({
    paste( input$entity.a, input$entity.b, sep = " " )
  })
  
  
  
  output$value <- renderText({ input$somevalue })
  output$transitivity <- renderText({ paste("The clustering coefficient of the given network is: ",transitivity(g.tidy.x())) })
  
  output$table <- renderDataTable(data.frame(Degree=degree(g.tidy.x()),
                                            Betweenness=betweenness(g.tidy.x()),
                                            Eigenvector=round(evcent(g.tidy.x())$vector, 2)),
                                  options=list(lengthMenu = c(5, 30, 50), pageLength = 5)
                                  )
  #)
  output$plot <- renderPlot({
    plot(g.tidy.x(),vertex.size = 0.05, vertex.label = NA)
  })
  output$table.entity_type <- renderDataTable(dt.by_type,options = list(lengthChange = FALSE,searching = FALSE,paging=FALSE))
  
  output$table.connection_type <- renderDataTable(head(dt.count.connections.order),options= list(lengthChange = FALSE,searching = FALSE,paging=FALSE))
  
  output$hist_degree <- renderPlot({
    ggplot(data=G.degree.histogram, aes(x=hist_degree, y=Freq)) +  geom_bar(stat="identity") + xlim(0,input$XLIM)},
    height=600,width=400)
  
  output$neighbor_plot <- renderPlot({
    plot(neighbors_plotting())
  })
  
  output$top.entity.connection.types.plot.plot <- renderPlot({
    plot(top.entity.connection.types.plot())
  })
  
  output$explore.subgraph<- renderPlot({
    plot(explore.subgraph(),layout=layout.fruchterman.reingold,
         vertex.label.cex = .5,
         vertex.size = 5,
         edge.arrow.size = .1)
  })
  
  output$topConnectionTypes <- DT::renderDataTable(top.entities.type.text(),options = list(lengthChange = FALSE,searching = FALSE,paging=FALSE))
  
  output$txtOut <- renderText(input$entity)
  
  output$predicted.links <- renderPlot({
    plot.predicted.links1()
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)

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
  
  # Change Names of Columns
  #colnames(dt.unique.entities.attributes) <- c("Name", "Entity_Type")
  #colnames(dt.all.entities.attributes) <- c("Name", "Entity_Type", "Entity_Count")
  
  #Relationship Table with Connection as Edge attribute
  #dt.trump.connections <- dt.trump[, c("Entity_A", "Entity_B", "Connection")]
  
  # Build undirected graph
  g.trump <- graph.data.frame(dt.trump.connections, directed = FALSE, vertices = dt.unique.entities.attributes)
  #plot(g.trump, vertex.size = 0.05, vertex.label = NA)
  
  by_type <- dt.unique.entities.attributes %>% count(Entity_Type)
  dt.by_type <- data.table(by_type)
  
  
  count.connections <- dt.trump.connections %>% count(Connection)

  count.connections.order <- count.connections[order(-count.connections$n,),]
  dt.count.connections.order <- data.table(count.connections.order)
  
  G.degree <- degree(g.trump)
  G.degree.histogram <- as.data.frame(table(G.degree))
  G.degree.histogram[,1] <- as.numeric( paste(G.degree.histogram[,1]))
  Degree_Hist <- ggplot(data=G.degree.histogram, aes(x=G.degree, y=Freq)) +  geom_bar(stat="identity") + coord_flip() + scale_y_log10()
  
  # Basic descriptive statistics
  #total.entities.type <- function(entity_type_category) {
    #dt.unique.entities.attributes[Entity_Type == entity_type_category, .N]
  #}
  #total.entities.person <- total.entities.type("Person")
  #total.entities.person <- total.entities.type("Organization")
  #total.entities.person <- total.entities.type("Federal Agency")
  #
  #top.entities.name <- function(top_number) {
    #unique(dt.all.entities.attributes[, entity_name_count := .N, by=Name][order(-entity_name_count)])[1:top_number]
 # }
  
  #Create top N by organization type
  
  # Create function to build subgraphs
  
  ##################################################################################################################################
  
  server <- function(input, output){
    
     output$plot <- renderPlot({
      
      entity_connection <- input$connection
      #entity.type.plot <- function(g.trump, entity_type_1, entity_type_2) {
        #entity_connection <- paste(entity_type_1, entity_type_2, sep="")
        entity_connection <- gsub("-","",entity_connection)
        el <- get.edgelist(g.trump)
        E(g.trump)$entity_type_connection <- paste(V(g.trump)[el[, 1]]$Entity_Type, V(g.trump)[el[, 2]]$Entity_Type, sep = "")
        if (entity_connection == "PersonPerson") {
          edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "PersonPerson")]
        } else if (entity_connection == "OrganizationOrganization") {
          edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "OrganizationOrganization")]
        } else if (entity_connection == "PersonOrganization")
          {edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == c("OrganizationPerson", "PersonOrganization"))]
        } else {
          edges.to.keep <- E(g.trump)
        }
        g.trump.filtered <- subgraph.edges(g.trump, eids = edges.to.keep, delete.vertices = TRUE)
        plot(g.trump.filtered,vertex.size = 0.05, vertex.label = NA)
        #V(g.trump.filtered)
        #plot(g.trump)
      #}
    
    })
     output$table1 <- renderDataTable(dt.by_type,options = list(lengthChange = FALSE,searching = FALSE,paging=FALSE))
     #output$table1 <- renderText(dt.by_type)
     output$table2 <- renderDataTable(dt.count.connections.order,options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
     output$DegreeTable <- renderDataTable(G.degree.histogram,options = list(lengthChange = FALSE,searching = FALSE,paging=FALSE))
     
     output$neighbour_plot <- renderPlot({
       entity <- input$entity
       g.neighbors.entity <- neighbors(g.trump, V(g.trump)$name == entity)
       g.neighbors <- induced.subgraph(g.trump, vids = (V(g.trump)%in% g.neighbors.entity) | (V(g.trump)$name == entity))
       V(g.neighbors)$label <- ''
       plot(g.neighbors, vertex.label.cex=0.8, vertex.label.dist=4)
     })
     
    output$DegreeHist <- renderPlot({
      plot(Degree_Hist)
    })
   
}

#UI TEST
library(shiny)
library(ggplot2)


#UI

ui <- navbarPage("TrumpWorld Data", 
  #theme = bs_theme(bg = "white",
  #                fg = "midnightblue",
  #               primary = "maroon",
  #             base_font = font_google("Montserrat")),
  
  tabPanel(
    "Descriptive Statistics", icon = icon("bar-chart-o"),
      sidebarLayout(
        sidebarPanel(

          # sliderInput(
          #   "integer", "Integer:",
          #   min = 0, max = 1000,
          #   value = 500
          # )

        ),
        mainPanel(
        #plotOutput(outputId = "plot"),
        dataTableOutput("table1"),
        dataTableOutput("table2"),
        plotOutput("DegreeHist"),
        dataTableOutput("DegreeTable")
        )
      )
  ),
  tabPanel(
    'Network Exploration',icon = icon("link", lib = "font-awesome"),
    sidebarLayout(
      sidebarPanel(
        selectInput("connection", 
                    label = "Choose a connection to display",
                    choices = c(
                      "Person-Person", 
                      "Organization-Organization",
                      "Person-Organization","All"
                    ),
                    selected = "Person-Person"),
        selectInput("Entity Type", 
                    label = "Choose an entity type",
                    choices = c("Person", 
                                "Organization",
                                "Federal Agency"),
                    selected = "Person-Person"),
        selectInput("entity", 
        label = "Choose an entity",unique.entities,
        selected = "ALABAMA POLICY INSTITUTE"),
      ),
      mainPanel(
        plotOutput(outputId = "neighbor_plot"),
        plotOutput(outputId = "plot")
        
          #dataTableOutput("table1"),
          #dataTableOutput("table2")
      )
    )
  ),
  tabPanel("Network Analysis", icon = icon("chart-line", lib = "font-awesome"))
)


shinyApp(ui, server)
# Run the application 
shinyApp(ui = ui, server = server)

#library(rsconnect)
#rsconnect::deployApp('C:/Users/ievap/OneDrive/Desktop/Mokslai/Network Ananlytics/Group work')

#Y

entity <- "ALABAMA POLICY INSTITUTE"
g.neighbors.entity <- neighbors(g.trump, V(g.trump)$name == entity)
g.neighbors <- induced.subgraph(g.trump, vids = (V(g.trump)%in% g.neighbors.entity) | (V(g.trump)$name == entity))
V(g.neighbors)$label <- ''
plot(g.neighbors, vertex.label.cex=0.8, vertex.label.dist=4)

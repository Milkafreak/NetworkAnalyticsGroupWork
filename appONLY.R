#sliderInput("top_number", "Top X Connections between given entity types",
#           min = 0, max = 25, value = 5
# ),


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

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("TrumpWorld Data",
                           
                           ###########################################     PANEL 1  ##########################################################################
                           tabPanel("Descriptive Statistics", icon = icon("bar-chart-o"),
                                  
                                    mainPanel(
                                      h2(p(tags$strong("Context"))),
                                      h4(p("The TrumpWorld dataset includes relationships of entities that are in any way connected to Donald J. Trump. Interestingly, D. J. Trump was a U.S. president with an extensive number of connections to influential people and organizations, that might have supported his campaign through lobbyists. Since D. J. Trump appointed numerous former lobbyists as members of the government, the campaign-supporting organizations might have influenced numerous legislative processes and leveraged D. J. Trumps promotions of his businesses.")),

                                      h4(p("The authors of this analysis found it interesting to dive deep into these connections to understand the entities involved and the structure of the network.")),
                                      
                                      h4(p("The dataset, which has been obtained from the source: https://github.com/BuzzFeedNews/trumpworld, includes 3,381 data points. To be precise, a data point corresponds to a connection, with specifications of both side entity types, names of the persons/organizations and connection type. The dataset includes 2,669 number of unique entities, including", tags$strong("2,015"), "organizations,", tags$strong ("640"),"people, and", tags$strong("14"), "federal agencies.")
                                         ),
                                      
                                      h2(p(tags$strong("Descriptive Statistics"))),
                                      h4(p("The descriptive statistics section provides an overview of the network, including the entity types, connection types and total number of observations. ")
                                         ),
                                      h4(p("President and Ownership type of connections are among the most common ones, with occurrences of 472 and 334, respectively. It is important to point that the dataset was not cleaned in terms of ownership types as there are too many instances that have a connection type attributable under a certain umbrella-name, but which differ in just one word and, therefore, are impossible to assign manually (i.e. umbrella-name: 'President', similar connections: 'President (as of 2016)', 'President and CEO', 'President and COO', 'President, director' and etc.)")
                                      ),
                                      dataTableOutput("table.entity_type"),
                                      dataTableOutput("table.connection_type"),
                                      plotOutput("barplot.connections"),
                                      plotOutput("barplot.entities")
                                    )
                                    
                           ),
                           ###########################################  PANEL 2  ##########################################################################
                           
                           navbarMenu("Network Exploration", icon = icon("link", lib = "font-awesome"),
                           
                           
                           tabPanel('Centrality Measures',icon = icon("link", lib = "font-awesome"),
                                    sidebarPanel(
                                      tags$h4("Choose the entity types you want to observe in the network"),
                                      checkboxInput("check_Person", "Person", TRUE),
                                      checkboxInput("check_Organization", "Organization", TRUE),
                                      checkboxInput("check_Federal_Agency", "Federal Agency", TRUE),
                                      ), 
                                    
                                    mainPanel(
                                      h2(p(tags$strong("Network Centrality Measures"))),
                                      
                                      h4("The clustering coefficient of the given network is: "),
                                      tags$code(verbatimTextOutput("transitivity")),
                                      h4("The average path length of the given network is: "),
                                      tags$code(verbatimTextOutput("avg.path.length")),
                                      h4("The diameter of the given network is: "),
                                      tags$code(verbatimTextOutput("diameter")),
                                      
                                      dataTableOutput("table"),
                                  
                                      h2(p(tags$strong("Network Visualization"))),
                                    
                                      plotOutput(outputId = "plot"),
                                      
                                      
                                      h2(p(tags$strong("Analysis")),
                                        h4("The Network Exploration part provides an interactive table with three centrality measures, i.e., degree, betweenness and eigenvectors, which the user can filter by number of entries and name of entity, and sort by selected centrality measure. The part also provides a visualization of the network distribution which can be filtered by entity type.")),
                                      h4("The key take-aways from the exploration is that Donald J. Trump positions highest in all three centrality measures, followed by Wilbur Ross, Steven Mnuchin, Jared Kushner, and Donald Trump JR; the two last people are a part of D. J. Trump's family. The table once again confirms that D. J. Trump family played a big role in his presidency and business governance, which resulted in multiple conflicts of interest.")
                                      
                                    )),
                           
                           tabPanel('Degree Distribution',icon = icon("link", lib = "font-awesome"),
                                    sidebarPanel(
                                      tags$h4("Adjust the slider to explore the histogram"),
                                      sliderInput("XLIM", "Histogram x-Axis Limit:",
                                                  min = 0, max = 757, value = 500
                                                  
                                      ), 
                                    ), 
                                    mainPanel(
                                      
                                      h2(p(tags$strong("Degree Distribution Histogram"))),
                                      
                                      h4(p("In this section, we let the user interact by allowing them to choose the x axis of the degrees they want to observe closer in the degree distribution histogram.")),
                                      h4(p("As expected, the network follows a Power-Law distribution, since majority of the networks have a degree of 1, and very few have a large number of degrees. D. J. Trump has the highest degree distribution - this is in line with his position of distrust of other people and organizations and his need to be directly in contact with all entities himself.")
                                         ),
                                        
                                      plotOutput("hist_degree"),
                                   
                                    )
                                
                                  )),
                           
                           
                           ###########################################     PANEL 3  ##########################################################################

                            navbarMenu("Network Analysis", icon = icon("chart-line", lib = "font-awesome"),       
                           
                            tabPanel("Link Prediction", icon = icon("chart-line", lib = "font-awesome"),
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
                                                selectInput("n_degree_pred",
                                                           label = "Choose a n_degree",c(1,2,3,4,5,6,7,8,9),
                                                          selected = "5")
                                              ),
                                              mainPanel(
                                                
                                                h2(p(tags$strong("Link Prediction"))),
                                                h4("Select entity type to predict future links."),
                                                h4("The following graph displays links from Trump's world that are predicted based on common neighbors:"),
                                              
                                                
                                                plotOutput(outputId = "predicted.links"),
                                                
                                                h2(p(tags$strong("Analysis")),
                                                h4("The Link Prediction subsection of Network Analysis displays a prediction of links based of common neighbors. The user can interact with the graph by selecting which type of entity types and assigned weights they wish to predict. ")),
                                                h4("For example, one of the most likely links between Person-Person (weight = 3) is between Ivanka Trump and Rupert Mudroch (Fox News President)"),
                                                h4("However, it is important to note that one of the drawbacks of the common neighbors algorithm is that it does not consider the relative number of common neighbors, thus it should be interpreted with caution.")
                                                
                                              )
                                              
                                     ),
                            
                            ############################################### PANEL 4 #############################################################################
                            
                            tabPanel("Explore Subgraph", icon = icon("chart-line", lib = "font-awesome"),
                                     sidebarPanel(
                                       selectInput("entity.a.expl",
                                                   label = "Choose an entity type for type A",c("Person","Organization"),
                                                   selected = "Organization")
                                       ,
                                       selectInput("entity.b.expl",
                                                   label = "Choose an entity type for type B",c("Person","Organization"),
                                                   selected = "Organization"),
                                
                                       selectInput("n_degree_expl",
                                                   label = "Choose minimum degree for which you would like to display labels",c(1,2,3,4,5),
                                                   selected = "3"),
                                       selectInput("connection_type",
                                                   label = "Choose connection type",dt.trump.connections$Connection,
                                                   selected = "Ownership"),
                                   
                                       
                                     ),
                                     mainPanel(
                                       
                                       
                                       
                                       h2(p(tags$strong("Subgraph Exploration"))),
                                       h4("Select entity and connection type to visualize the largest cluster within that network."),
                                      
                                       plotOutput(outputId = "explore.subgraph.plot"),
                                       
                                       h2(p(tags$strong("Analysis")),
                                        h4("The Subgraph Exploration part allows the user to select entity and connection type to visualize the largest cluster within that network.")),
                                       
                                       h4("We consider that an interesting example for this analysis is to see all organizations that own other organizations. In this case, the organization with most holdings is DJT Holdings LLC. We decided to use a cluster function to investigate this specific subnetwork with the decompose function, so that at the end it returns the largest of all the individual subnetworks."),
                                       h4("For this specific example that we recommend for visualization (although the user is free to explore any), we see a star network. DJT Holdings LLC is at the center of the star, and controls all the rest (is a chain of ownership). This is interesting since it tells a lot about the business style of Donald Trump. It can be inferred that this is part of his leadership style, as well as his need to oversee every action of his empire.")

                                     )
                            

                            ),
                            ######################################### PANEL 5   ###############################################
                             
                            tabPanel("Neighborhood Exploration", icon = icon("chart-line", lib = "font-awesome"),
                                     sidebarPanel(
                                       selectInput("entity", 
                                                   label = "Choose an entity",unique.entities,
                                                   selected = "ALABAMA POLICY INSTITUTE"),
                                       
                                     ),
                                     mainPanel(
                                       
                                       h2(p(tags$strong("Neighborhood Exploration"))),
                                       h4("Select an entity to see their neighbors."),
                                       verbatimTextOutput("txtOut"),
                                       
                                       plotOutput(outputId = "neighbor_plot"),
                                       
                                       h2(p(tags$strong("Analysis")),
                                        h4("The Neighborhood Exploration subsection allows the user the closely view the neighbors of a selected entity. Although, many of entities have 2-3 neighbors, larger network players, such as Donald J. Trump, Donald Trump JR display interesting graphs with neighbors barely having any connection between each other - the links are only to the selected node itself."))
                                     )
                                     
                            )
                           
                           ###############################################################################################################
                ) 
)
)



# Define server function  
server <- function(input, output) {
  
  #######################################################################   REACTIVES   ##########################################################################  
  
  # 
  g.tidy.x <- reactive({
    df[df["Type"] == "Person",]$Appear = input$check_Person
    df[df["Type"] == "Organization",]$Appear = input$check_Organization
    df[df["Type"] == "Federal Agency",]$Appear = input$check_Federal_Agency
    g.tidy.x <- g.tidy %>% activate(nodes) %>% filter(entity_type %in% c(df[df["Appear"] == TRUE,]$Type))
    g.tidy.x
  })
  
  # Create a subgraph that only shows the connection of the chosen entity types
  entity.type.plot <- reactive({
    entity_connection <- paste(input$entity.a, input$entity.b, sep=" ")
    if (entity_connection == "Person Person") {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "Person Person")]
    } else if (entity_connection == "Organization Organization") {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "Organization Organization")]
    } else {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == c("Organization Person", "Person Organization"))]
    } 
    g.trump.filtered <- subgraph.edges(g.trump, eids = edges.to.keep, delete.vertices = TRUE)
    g.trump.filtered
  })
  
  # Create a dataframe with the top N entities with more connections
  top.entities.type.text <- reactive({
    df.top321 <- unique(dt.trump[Entity_A_Type == input$entity.a][Entity_B_Type == input$entity.b][, count_connections := .N, by = "Connection"]
                        [order(-count_connections)], by = "Connection")[1:input$top_number][, !c("Entity_A", "Entity_B", "Sources")]
    df.top123 <- data.frame(df.top321)[c("Connection","count_connections")]
    return(df.top123)
  })
  
  # Create a link prediction
  plot.predicted.links1 <- reactive({
    
    g.entity.type <- entity.type.plot() 
    m.predicted.edges <- as.matrix(cocitation(g.entity.type) * (1-get.adjacency(g.entity.type)))
    g.predicted.edges <- graph_from_adjacency_matrix(m.predicted.edges,
                                  mode = "undirected",
                                  weighted = TRUE)
    E(g.predicted.edges)$width <- E(g.predicted.edges)$weight * 2
    edges.to.keep <- E(g.predicted.edges)[which(E(g.predicted.edges)$weight == input$weight)]
    g.weighted.edges <- subgraph.edges(g.predicted.edges, edges.to.keep, delete.vertices = TRUE)
    #g.weighted.edges <- plot(g.weighted.edges,vertex.size = 5, vertex.label = ifelse(degree(g.weighted.edges) >= input$n_degree_pred, V(g.weighted.edges)$name, NA))
    g.weighted.edges
  })
  
  # Create a subgraph that only shows the connection of the chosen connection type
  top.entity.connection.types.plot <- reactive ({
    vertices.to.delete <- V(g.trump)[which(V(g.trump)$Entity_Type != input$EntityType)]
    gtrump.subgraph <- delete.vertices(g.trump, vertices.to.delete)
    edges.to.keep <- E(gtrump.subgraph)[which(E(gtrump.subgraph)$Connection == input$connection_type)]
    g.trump.filtered <- subgraph.edges(gtrump.subgraph, eids = edges.to.keep, delete.vertices = TRUE)
    plot(g.trump.filtered, vertex.size = 0.05, vertex.label = ifelse(degree(g.trump.filtered) >= input$n_degree, V(g.trump.filtered)$name, NA))
    g.trump.filtered
  })
  
  # Create a subgraph that only shows the entity with higher degree of the chosen connection type
  
  explore.subgraph <- reactive({
    entity_connection <- paste(input$entity.a.expl, input$entity.b.expl, sep=" ")
    if (entity_connection == "Person Person") {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "Person Person")]
    } else if (entity_connection == "Organization Organization") {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == "Organization Organization")]
    } else {
      edges.to.keep <- E(g.trump)[which(E(g.trump)$entity_type_connection == c("Organization Person", "Person Organization"))]
    } 
    g.entity.type1 <- subgraph.edges(g.trump, eids = edges.to.keep, delete.vertices = TRUE)
    
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
  
  # Create a subgraph that plots the neighbors of a given entity
  neighbors_plotting <- function() {
    entity <- input$entity
    g.neighbors.entity <- neighbors(g.trump, V(g.trump)$name == entity)
    g.neighbors <- induced.subgraph(g.trump, vids = (V(g.trump)%in% g.neighbors.entity) | (V(g.trump)$name == entity))
    g.neighbors
  }
  
  #######################################################################   OUTPUTS   ##########################################################################  
  
  output$txtout <- renderText({
    paste( input$entity.a, input$entity.b, sep = " " )
  })
  
  
  
  output$value <- renderText({ input$somevalue })
  output$transitivity <- renderText({transitivity(g.tidy.x()) })
  output$diameter <- renderText({diameter(g.tidy.x()) })
  output$avg.path.length <- renderText({mean_distance(g.tidy.x()) })
  
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
    ggplot(data=degree.histogram, aes(x=hist_degree, y=Freq)) +  geom_bar(stat="identity") + xlim(0,input$XLIM)},
    height=600,width=400)
  
  output$neighbor_plot <- renderPlot({
    plot(neighbors_plotting())
  })
  
  output$top.entity.connection.types.plot.plot <- renderPlot({
    plot(top.entity.connection.types.plot())
  })
  
  output$explore.subgraph.plot<- renderPlot({
    plot(explore.subgraph(),
         layout=layout.fruchterman.reingold,
         vertex.label.cex = .5,
         vertex.size = 5,
         edge.arrow.size = .1,
         vertex.label = ifelse(degree(explore.subgraph()) >= input$n_degree_expl, V(explore.subgraph())$name, NA))
         
  })
  
  output$topConnectionTypes <- DT::renderDataTable(top.entities.type.text(),options = list(lengthChange = FALSE,searching = FALSE,paging=FALSE))
  
  output$txtOut <- renderText(input$entity)
  
  output$predicted.links <- renderPlot({
    plot(plot.predicted.links1(),
    #layout=layout.fruchterman.reingold,
    vertex.label.cex = .5,
    vertex.size = 5,
    edge.arrow.size = .1,
    vertex.label = ifelse(degree(plot.predicted.links1()) >= input$n_degree_pred, V(plot.predicted.links1())$name, NA))
  })
  
  output$barplot.connections <- renderPlot({bar.plot.connections
  })
    
  output$barplot.entities <- renderPlot({bar.plot.entities
  })
    
} # server

source('global.R', local = TRUE)

# Create Shiny object
shinyApp(ui = ui, server = server)
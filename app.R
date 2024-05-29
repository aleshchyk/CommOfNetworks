# Load packages ----
library(shiny)

# Source helpers ----
source("helpers.R")

ui <- fluidPage(
  titlePanel("Multi-Omics Bayesian Community of Networks"),
  #helpText(
  #  "Obtain the distribution of a set of variables given finding (evidence) on other variables"
  #),
  #helpText("Pr(Y, Z | X = a)"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      #helpText("Select a network"),
      selectInput(
        "Obj",
        label = "Choose a network object",
        choices = c(
          "Syntegra data",
          "Tryprophan pathway-NECS",
          "Tryprophan pathway-LLFS",
          "Tryprophan pathway-NECS-greedy-search",
          "Tryprophan pathway-LLFS-greedy-search",
          "NECS",
          "NECS-genetics",
          "LLFS-age-q1",
          "LLFS-age-q2",
          "LLFS-age-q3",
          "LLFS-age-q4"
        ),
        selected = NULL
      ),
      selectInput(
        "testNetwork",
        label = "Choose a network",
        choices = slotNames(obj),
        selected = NULL
      ),
      
    ),
    
    mainPanel(
      width = 12,
      height = 2,
      tabsetPanel(
        tabPanel(
          "Network visualization",
          textOutput("selected_network"),
          visNetworkOutput("vizNet")
        ),
        tabPanel(
          "Set evidence",
          
          
          helpText(
            "Obtain the distribution of a set of variables given finding (evidence) on other variables"
          ),
          helpText("Pr(Y | X = a)"),
          
          selectInput(
            "eviNode",
            label = "Choose a node (X)",
            choices = names(test_network),
            selected = NULL
          ),
          
          
          
          helpText("Select a node for which the distribution is requested"),
          selectInput(
            "node_2",
            label = "Choose a node to display (Y)",
            choices = names(test_network),
            selected = NULL
          ),
          
          
          
          helpText("Select the type of distribution"),
          selectInput(
            "distType",
            label = "Choose distribution type",
            choices = c("conditional", "joint"),
            selected = "joint"
          ),
          
          
          tabPanel(
            "Probability Table",
            helpText("Probability table"),
            tableOutput("table"),
            helpText("Community of Networks Probability Table"),
            tableOutput("community_table")
          ),
          
        ),
        
        tabPanel(
          "Markov Blanket of a node",
          helpText("Markov Blanket table"),
          downloadButton("downloadMB", "Download Markov Blanket as CSV"),
          tableOutput("MB_table")
          
          
        ),
        tabPanel(
          "Marginal distribution of a node",
          helpText("Select a node for which the distribution is requested"),
          selectInput(
            "node_4",
            label = "Choose a node to display",
            choices = names(test_network),
            selected = NULL
          ),
          tableOutput("marginal_table")
          
        ),
        
      )
    )
    
    
  )
)



server <- function(input, output, session) {
  observeEvent(input$Obj, {
    obj <- getNetObj(input$Obj)
    test_network <- getTestNetwork(obj, input$testNetwork)
    choices_ = names(test_network)
    choices = c(
      "Syntegra data",
      "Tryprophan pathway-NECS",
      "Tryprophan pathway-LLFS",
      "Tryprophan pathway-NECS-greedy-search",
      "Tryprophan pathway-LLFS-greedy-search",
      "NECS",
      "NECS-genetics",
      "LLFS-age-q1",
      "LLFS-age-q2",
      "LLFS-age-q3",
      "LLFS-age-q4"
    )
    updateSelectInput(session, "testNetwork", choices = slotNames(obj))
    updateSelectInput(session, "eviNode", choices = choices_)
    updateSelectInput(session, "node_2", choices = choices_)
    updateSelectInput(session, "eviNode1", choices = choices)
    updateSelectInput(session, "eviNode2", choices = choices)
    updateSelectInput(session, "node_4", choices = choices_)
    
  })
  observeEvent(input$testNetwork, {
    obj <- getNetObj(input$Obj)
    test_network <- getTestNetwork(obj, input$testNetwork)
    choices = slotNames(obj)
    updateSelectInput(session, "eviNode")
    updateSelectInput(session, "eviNode1", choices = choices)
    updateSelectInput(session, "eviNode2", choices = choices)
    updateSelectInput(session, "node_2")
    #updateSelectInput(session, "node_3", choices = choices)
    updateSelectInput(session, "node_4")
    
  })
  
  observeEvent(input$eviNode, {
    message("Evidence input event observed eviNode")
    obj <- getNetObj(input$Obj)
    test_network <- getTestNetwork(obj, input$testNetwork)
    choices <- junction$universe$levels[[input$eviNode]]
    updateSelectInput(session, "value", choices = choices)
  })
  
  observeEvent(input$eviNode1, {
    message("Evidence input event observed eviNode1")
    obj <- getNetObj(input$Obj)
    test_network <- getTestNetwork(obj, input$testNetwork)
    junction <- compile(as.grain(test_network))
    choices1 <- junction$universe$levels[[input$eviNode1]]
    updateSelectInput(session, "value1", choices = choices1)
  })
  
  observeEvent(input$eviNode2, {
    message("Evidence input event observed eviNode2")
    obj <- getNetObj(input$Obj)
    test_network <- getTestNetwork(obj, input$testNetwork)
    junction <- compile(as.grain(test_network))
    choices2 <- junction$universe$levels[[input$eviNode2]]
    updateSelectInput(session, "value2", choices = choices2)
  })
  
  output$table <- renderTable({
    getProbTable(
      input$Obj,
      input$testNetwork,
      input$eviNode,
      input$node_2,
      input$distType,
      input$node_4
    )
  }, rownames = TRUE)
  
  output$community_table <- renderTable({
    getCommunityNetworkTable(
      input$Obj,
      input$testNetwork,
      input$eviNode,
      input$node_2,
      input$distType,
      input$node_4
    )
  }, rownames = TRUE)
  
  output$marginal_table <- renderTable({
    getMargProbTable(input$Obj,
                     input$testNetwork,
                     input$node_4)
  }, rownames = TRUE)
  
  output$MB_table <- renderTable({
    getMBTable(input$Obj)
  }, rownames = TRUE)
  
  output$vizNet <- renderVisNetwork({
    data_ann <- getVizNetDataAnn(input$Obj, input$testNetwork)
    
    
    nodes2 <-
      data.frame(data_ann$nodes,
                 data_ann$nodes,
                 font.size = 33)
    edges2 <- data.frame(data_ann$edges, width = 0.8)
    
    set.seed(9001)
    visNetwork(nodes = nodes2,
               edges = edges2,
               height = "100%",
               width = "100%") %>%
               visInteraction(navigationButtons = TRUE) %>%
                 # visGroups(groupname = "ProtRS", color = "lightblue") %>%
                 # visGroups(groupname = "MetRS", color = "lightgreen") %>%
                 # visGroups(groupname = "phenotype", color = "magenta") %>%
                 # visGroups(groupname = "SNP", color = "yellow")  %>%
                 # visGroups(groupname = "metabolite", color = "lightgreen")  %>%
                 # visGroups(groupname = "enzyme", color = "lightpink")  %>%
                 visEdges(
                   shadow = TRUE,
                   arrows = list(to = list(
                     enabled = TRUE, scaleFactor = 2
                   )),
                   color = list(color = "black", highlight = "red")
                 ) %>%
                 visLegend() %>%
                 visOptions(
                   #selectedBy = "group",
                   highlightNearest = list(enabled = TRUE, hover = TRUE),
                   nodesIdSelection = TRUE
                 ) %>%
                 visIgraphLayout(layout = 'layout_nicely')
  })
  output$downloadMB <- downloadHandler(
    filename = function() {
      paste("Markov_Blanket", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getMBTable(input$Obj), file, row.names = FALSE)
    }
  )
    
}
  


shinyApp(ui, server)

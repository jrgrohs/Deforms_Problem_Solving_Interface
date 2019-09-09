#
# Deforms Problem Solving Interface
# April 2019
#

# Libraries ---- 
# source("https://install-github.me/zzawadz/dragulaR")  -  drag and drop library

# This depends on a modified version of the dragulaR library,
# until zzawadz accepts my pull request, and the updates make their way to CRAN,
# install the latest version from github:
# devtools::install_github("https://github.com/hazybluedot/dragulaR")

library(shiny)
library(tidyverse)
library(knitr)
#library(shinyWidgets)
library(dragulaR)

#source("R/debounce.R")
source("R/workspace.R")
source("R/workspaceUI.R")

# Define makeElement
# creates the div containers for the drag and drop
makeElement <- function(data)
{
    #print(str(data))
	name <- data[which(names(data) == "name")] #names(data)[[1]]
	div(style = "border-width:2px;border-style:solid;",
		drag = name,
		class = "active content", paste0('$$', data[which(names(data) == "eq")], '$$'))
}


# Define UI
ui <- fluidPage(theme = "bootstrap.min.css",
  
  #fluidRow(column(5, offset = 4, titlePanel("Deforms Problem Solving Interface"))),
  tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  titlePanel("Deforms Problem Solving Interface"),
  
  sidebarLayout( position = "right",
                 
                 sidebarPanel(
                   h4("Equation Bank"),
                   column(12,
                          h3("Drag from here:"),
                          uiOutput("equationList")
                   ),
                   
                   hr(),
                   
                   h4("Solution and Feedback")
                 ),
                 
                 mainPanel(
                   fluidRow( 
                     column(12,
                            withMathJax(),
                            uiOutput("problemStatement")
                     )
                   ),
                   
                   # Space betweeen Problem and Workspace
                   br(),
                   br(),
                   
                   fluidRow(
                       column(12,
                              workspaceUI("main", "Main Work Space")#,
                              #workspaceUI("side", "Side workspace")
                       )
                       
                   )#,
                   #actionButton("solve", "Solve"),
                   #fluidRow(
                #       verbatimTextOutput("solution"),
                       #h4("Debug Info"),
                       #verbatimTextOutput("debug"),
                       #h5("workspace"),
                       #verbatimTextOutput("workspace_dump")
                   #)
                 )
  ),
  dragulaOutput("dragula")
)

# Define Server Function

server <- function(input, output, session) {
  selectedProblem <- file.path('problems', 'problem1.md')
  workspace = reactiveValues(data = initializeWorkspace(), 
                             model = c(), equations = c(),
                             subs = list())
  #subs = reactiveValues()
  
  obsList <- list()
  workspaces <- list()

  # output$items <- renderPrint({
  #     dragulaValue(input$dragula)
  # })
  
  output$workspace <- renderPrint({
    reactiveValuesToList(workspace)
  })
  
  observeEvent(selectedProblem, {
      frontMatter <- rmarkdown::yaml_front_matter(selectedProblem)
      rows <- lapply(frontMatter$equations, function(r) { 
          data.frame(name = names(r)[[1]], eq = r[[1]])
      })
      workspace$equations <- do.call(rbind, rows)
  })
  
  workspaces[["main"]] <- callModule(module = workspaceModule, id = "main", 
                                     drop = reactive(workspaceValue(input$dragula, "main")),
                                     equationBank = workspace$equations)
  
  output$problemStatement <- renderUI({
    HTML(markdown::markdownToHTML(knit(selectedProblem, quiet = TRUE)))
  })
  
  output$equationList <- renderUI({
    #vars$frontMatter <- rmarkdown::yaml_front_matter(selectedProblem)
    div(id = "Available", style = "min-height: 250px;", 
        apply(workspace$equations, 1, makeElement))
  })
  
  # Dragula Functions
  output$dragula <- renderDragula({
    dragula(c("Available", "main-workspace"), copyOnly = 'Available', removeOnSpill = TRUE)
  })
  
  output$debug <- renderPrint(input$dragula)
  output$print <- renderText({
    state <- dragulaValue(input$dragula)
    sprintf("Available:\n  %s\n\nModel:\n  %s",
            paste(state$Available, collapse = ", "),
            paste(state$Model, collapse = ", "))
    
  })
  
  observeEvent(input$solve, {
      message("main app solve it!")
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Thank you for clicking')
  })
  
  output$workspace_dump <- renderPrint(reactiveValuesToList(workspace))
}

# Run the application

shinyApp(ui = ui, server = server)


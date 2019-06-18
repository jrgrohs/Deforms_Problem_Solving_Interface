#
# Deforms Problem Solving Interface
# April 2019
#

# Libraries ----
# source("https://install-github.me/zzawadz/dragulaR")  -  drag and drop library
library(shiny)
library(knitr)
library(tidyverse)
library(shinyWidgets)
library(png)
library(dragulaR)

# Define Variables 
var_list <- c("Unknown","50 mm","50 mm","1 mm","70 GPa","23 x 10-6 oC-1","100 oC")

# Define makeElement
# creates the div containers for the drag and drop
makeElement <- function(data)
{
  name <- names(data)[[1]]
  div(style = "border-width:2px;border-style:solid;",
      drag = name,
      div(class = "active content", withMathJax(paste0('\\(', data, '\\)'))))
}


# Define UI
ui <- fluidPage(
  
  #fluidRow(column(5, offset = 4, titlePanel("Deforms Problem Solving Interface"))),
  titlePanel("Deforms Problem Solving Interface"),
    
  sidebarLayout( position = "right",
    
    sidebarPanel(
      h4("Equation Bank"),
      column(12,
             h3("Drag from here:"),
             uiOutput("equationList")
             
             # TODO: Fix Persistance in Drag and Drop elements
             #    Should have to do with setting the copy variable true, 
             #    but that documentation is only for html
             #    https://sindu12jun.github.io/dragula/
             # ,
                 # lapply(colnames(mtcars), makeElement, data = mtcars)
             # dragula([document.getElementById("Available")),
             # document.getElementById("Available")],{
             #   copy:true
             # }
        ),
      dragulaOutput("dragula"),
      
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
       #  column(4, 
       #    wellPanel(
       #      h4("Working Equations"),
       #      dropdownButton(
       #        tags$h3("Equation 1"), uiOutput("testing2"),
       #        circle = FALSE, status = "primary", label=pluck(temp_change_formula,formula), 
       #        icon = icon("calculator"), 
       #        width = "300px",
       #        tooltip = tooltipOptions(title = "Click to select your variables!")
       #      ) 
       #    )
       #  ),

      column(12,
        h3("Work Space"),
        column(12,
          h3("Drop here:"),
          div(id = "Model", style = "min-height: 600px;")
          )
        )
      )
    )
  )
)


# Define Server Function

server <- function(input, output) {
   
  selectedProblem <- file.path('problems', 'problem1.md')
  
  output$problemStatement <- renderUI({
    HTML(markdown::markdownToHTML(knit(selectedProblem, quiet = TRUE)))
  })
  
  output$equationList <- renderUI({
    frontMatter <- rmarkdown::yaml_front_matter(selectedProblem)
    div(id = "Available", style = "min-height: 250px;", 
      lapply(frontMatter$equations, makeElement))
  })

  output$testing = renderUI({
    withMathJax() 
    materialSwitch(
      "area_formula", 
      label = withMathJax('test \\(e^{i \\pi} + 1 = 0\\)'),
      value = FALSE, 
      inline=TRUE)
    })
  
  output$testing2 = renderUI({
    withMathJax() 
    materialSwitch(
      "area_formula", 
      label = withMathJax('test \\(e^{i \\pi} + 1 = 0\\)'),
      value = FALSE, 
      inline=TRUE)
    })
  
  # Dragula Functions
  output$dragula <- renderDragula({
    dragula(c("Available", "Model"))
  })
  
  output$print <- renderText({
    state <- dragulaValue(input$dragula)
    sprintf("Available:\n  %s\n\nModel:\n  %s",
            paste(state$Available, collapse = ", "),
            paste(state$Model, collapse = ", "))
    
  })
  
}

# Run the application

shinyApp(ui = ui, server = server)


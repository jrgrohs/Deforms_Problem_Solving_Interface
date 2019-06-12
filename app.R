#
# Deforms Problem Solving Interface
# April 2019
#

# Libraries ----
# source("https://install-github.me/zzawadz/dragulaR")  -  drag and drop library
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(png)
library(dragulaR)


# Define Formulas
# Used for drag and drop equations
# TODO: Get the math to display properly
#   played around with Mathjax https://github.com/mathjax/mathjax
equation_1 <- c("delta=delta_T * l * $(\\alpha$)","$(\\beta$)")
equation_2 <- c("equation 2")
equation_3 <- c("equation 3")
physics_equations_df <- data.frame(equation_1, 
                                   equation_2,
                                   equation_3)

# Define Variables 
var_list <- c("Unknown","50 mm","50 mm","1 mm","70 GPa","23 x 10-6 oC-1","100 oC")
source("deforms_formulas.R")


# Define makeElement
# creates the div containers for the drag and drop
makeElement <- function(data, name)
{
  div(style = "border-width:2px;border-style:solid;",
      drag = name,
      div(class = "active content", p(sprintf("%s", data[[name]]))))
      # math(data[[name]])
      # div(class = "active content", p(sprintf("Class: %s", class(data[[name]])))))
}


# Define UI
ui <- fluidPage(
  
  #fluidRow(column(5, offset = 4, titlePanel("Deforms Problem Solving Interface"))),
  titlePanel("Deforms Problem Solving Interface"),
    
  sidebarLayout( position = "right",
    
    sidebarPanel(
      h4("Equation Bank"),
      dropdownButton(
        tags$h3("List of Equations"), 
        uiOutput("testing2"),
        circle = FALSE, status = "primary", 
        label = pluck(temp_change_formula, formula),
        icon = icon("calculator"), width = "300px",
        tooltip = tooltipOptions(title = "Click to select your equations!")
        ), 
      
      column(12,
             h3("Drag from here:"),
             div(id = "Available", style = "min-height: 250px;",
                 lapply(colnames(physics_equations_df), makeElement, data = physics_equations_df))
             
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
          h3("Problem 1"),
          
          # Deforms Problem
          p("A solid square bar of cross-sectional dimensions  50mm x 50 mm is attached 
                  rigidly to the wall at point A and has a gap of 1 mm  from the rigid wall 
                  at point B at \\(\\ 25^\\circ C\\)."),
          imageOutput("image1", height = "100%"),
          
          p("Knowing the bar is made of aluminum E = 70 GPa, \\(\\alpha = 23 * 10^{-6}\\) :"),
          p("a.  At what temperature does the gap close?"),
          p("b.  What stress exists in the bar at a temperature of \\(\\ 100^\\circ C \\)?")
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
   
  output$image1 <- renderImage({
    list(
      src = "deforms_figure.png",
      contentType = "image/png",
      alt = "This is a figure representing the bar fixed at one end and gapped on the other")}, 
    deleteFile=FALSE)
  
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


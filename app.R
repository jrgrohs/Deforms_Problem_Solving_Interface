#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(png)
library(shinyWidgets)
var_list=c("Unknown","50 mm","50 mm","1 mm","70 GPa","23 x 10-6 oC-1","100 oC")
withMathJax()
formulas=c("delta=delta_T * l * $(\\alpha$)","$(\\beta$)","eq2","eq3")
# Define UI for application that draws a histogram
ui <- fluidPage(
     fluidRow(column(12,offset=3,titlePanel("Deforms Problem Solving Interface"))),
     fluidRow(
       column(8,h4("Problem 1"),
       withMathJax(),
       helpText('A solid square bar of cross-sectional dimensions  50mm x 50 mm is attached rigidly to the wall at point A and has a gap of 1 mm  from the rigid wall at point B at \\(\\ 25^\\circ C\\).  Knowing the bar is made of aluminum E = 70 GPa, \\(\\alpha = 23 * 10^{-6}\\)  :a.  At what temperature does the gap close? b.  What stress exists in the bar at a temperature of \\(\\ 100^\\circ C \\)?'),
       imageOutput("image1",height=300)
       ),
       column(4,wellPanel(
         h4("Equation Bank"),
                hr(),
                selectInput('eqn_list', 'Equations (can select multiple)', c(Choose='',formulas), multiple=TRUE, selectize=TRUE),
                verbatimTextOutput('selected_eqn')
         ),
         hr()
       ),
       wellPanel(
         checkboxInput("temp_formula", label = "\\[alpha\\]", value = FALSE),
         #verbatimTextOutput("temp_formula_value"),
         conditionalPanel("input.temp_formula == 1",
                          selectInput('del_l', 'Delta l', c(Choose='', var_list),selected="Unknown", selectize=FALSE),
                          verbatimTextOutput('del_l'),
                          selectInput('l', 'l', c(Choose='', var_list),selected="Unknown", selectize=FALSE),
                          verbatimTextOutput('l'),
                          selectInput('del_T', 'Delta T', c(Choose='', var_list),selected="Unknown", selectize=FALSE),
                          verbatimTextOutput('del_T'),
                          selectInput('alpha_coef', 'Alpha Coef', c(Choose='', var_list),selected="Unknown", selectize=FALSE),
                          verbatimTextOutput('alpha_coef')),
         hr(),
         #checkboxInput("area_formula", label = "Area Formula", value = FALSE),
         #verbatimTextOutput("area_formula_value"),
         hr(),
         uiOutput("testing")
      ),
     dropdownButton(
       tags$h3("List of Equations"),
       circle = FALSE, status = "primary", label="label me",icon = icon("calculator"), width = "300px",
       tooltip = tooltipOptions(title = "Click to select your equations!")
       #uiOutput("testing"))
     ))
   )



# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$image1 <- renderImage({list(src = "deforms_figure.png",
                                     contentType = "image/png",
                                     alt = "This is a figure representing the bar fixed at one end and gapped on the other")
  },
    deleteFile=FALSE
    )
  output$selected_eqn <- renderPrint(input$eqn_list)
  output$temp_formula_value <- renderPrint({input$temp_formula})
  output$area_formula_value <- renderPrint({input$area_formula})
  output$del_l <- renderPrint(input$del_l) 
  output$l <- renderPrint(input$l) 
  output$del_T <- renderPrint(input$del_T) 
  output$alpha_coef <- renderPrint(input$alpha_coef) 
  output$testing = renderUI({
    withMathJax() 
    materialSwitch("area_formula", label = 'test \\(e^{i \\pi} + 1 = 0\\)',value = FALSE, inline=TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#

library(shiny)
library(tidyverse)
library(shinyWidgets)
var_list <- c("Unknown","50 mm","50 mm","1 mm","70 GPa","23 x 10-6 oC-1","100 oC")
source("deforms_formulas.R")
formulas <- c("delta=delta_T * l * $(\\alpha$)","$(\\beta$)","eq2","eq3")


ui <- fluidPage(
     fluidRow(column(12,offset=3,titlePanel("Deforms Problem Solving Interface"))),
     fluidRow(
       column(8,h4("Problem 1"),
       withMathJax(),
       helpText('A solid square bar of cross-sectional dimensions  50mm x 50 mm is attached rigidly to the wall at point A and has a gap of 1 mm  from the rigid wall at point B at \\(\\ 25^\\circ C\\).  Knowing the bar is made of aluminum E = 70 GPa, \\(\\alpha = 23 * 10^{-6}\\)  :a.  At what temperature does the gap close? b.  What stress exists in the bar at a temperature of \\(\\ 100^\\circ C \\)?'),
       imageOutput("image1",height = "100%")),
       column(4,wellPanel(
         h4("Equation Bank"),
         dropdownButton(
           tags$h3("List of Equations"),uiOutput("testing2"),
           circle = FALSE, status = "primary", label=pluck(temp_change_formula,formula),icon = icon("calculator"), width = "300px",
           tooltip = tooltipOptions(title = "Click to select your equations!")),
         hr()
       ))),
       fluidRow(
         column(2,wellPanel(h4("Working Equations"),
                dropdownButton(
                  tags$h3("Equation 1"),uiOutput("testing2"),
                  circle = FALSE, status = "primary", label=pluck(temp_change_formula,formula),icon = icon("calculator"), width = "300px",
                  tooltip = tooltipOptions(title = "Click to select your variables!")
                  ) 
                )),
         column(6,
           h4("Work Space")
         ),
         column(4,
                wellPanel(
           h4("Solution and Feedback")
                          )
        )
)
)


server <- function(input, output) {
   
  output$image1 <- renderImage({list(src = "deforms_figure.png",
                                     contentType = "image/png",
                                     alt = "This is a figure representing the bar fixed at one end and gapped on the other")
  },
    deleteFile=FALSE
    )
  output$testing = renderUI({
    withMathJax() 
    materialSwitch("area_formula", label = withMathJax('test \\(e^{i \\pi} + 1 = 0\\)'),value = FALSE, inline=TRUE)
  })
  output$testing2 = renderUI({
    withMathJax() 
    materialSwitch("area_formula", label = withMathJax('test \\(e^{i \\pi} + 1 = 0\\)'),value = FALSE, inline=TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


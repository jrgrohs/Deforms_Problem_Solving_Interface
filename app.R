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
library(knitr)
#library(shinyWidgets)
library(dragulaR)

source("R/workspace.R")

# Define Variables 
var_list <- c("Unknown","50 mm","50 mm","1 mm","70 GPa","23 x 10-6 oC-1","100 oC")

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
                     column(12,
                            h3("Work Space"),
                            column(12,
                                   h3("Drop here:"),
                                   #div(id = "Model", style = "min-height: 600px;"),
                                   uiOutput("Model", style = "min-height: 300px;")
                            )
                     )
                   ),
                   fluidRow(
                       verbatimTextOutput("items"),
                       verbatimTextOutput("workspace")
                   )
                 )
  )
)

# Define Server Function

server <- function(input, output) {
  selectedProblem <- file.path('problems', 'problem1.md')
  workspace = reactiveValues(data = initializeWorkspace(), model = c(), equations = c(), subs = data.frame(idx = integer(), value = numeric()))
  solver = reactiveValues(subs = list())
  
  obsList <- list()
  
  output$items <- renderPrint({
      dragulaValue(input$dragula)
  })
  
  output$workspace <- renderPrint({
    workspace$data
  })
  
  observeEvent(selectedProblem, {
      frontMatter <- rmarkdown::yaml_front_matter(selectedProblem)
      rows <- lapply(frontMatter$equations, function(r) { 
          data.frame(name = names(r)[[1]], eq = r[[1]])
      })
      workspace$equations <- do.call(rbind, rows)
  })
  
  observeEvent(input$dragula, {
      workspace <- refreshWorkspace(workspace, dragulaValue(input$dragula)$Model)
  })
  
  observeEvent(workspace$subs, {
      print(workspace$subs)
      subs <- apply(workspace$subs, 1, function(sub) {
           message("sub")
            idx = sub[1]
            if (!is.na(sub[2]) & is.numeric(sub[2])) {
                paste0("subs(", workspace$data[idx,]$sympy, ", ", sub[2], ")")
            } else {
                ""
            }
      })
      subs <- subs[str_length(subs) > 0]
      
      if (length(subs) > 0) {
          subs <- paste(subs, collapse = ".")
          print(subs)
          apply(workspace$data %>% filter(type == "expression"), 1, function(exp) {
              print(paste0("solve(", exp["sympy"], ".", subs, ")"))
          })
          
      }
  })
  
  output$problemStatement <- renderUI({
    HTML(markdown::markdownToHTML(knit(selectedProblem, quiet = TRUE)))
  })
  
  output$Model <- renderUI({
      if (nrow(workspace$data) == 0) {
          return("")
      }
      workspace$data %>% 
          mutate(Group = group_indices(., id)) %>% 
          split(.$Group) ->
          chunks
      #message("chunks:")
      #print(chunks)
      list(withMathJax(),
      lapply(chunks, function(df) {
              message("grouped df:")
              str(df)
              n = names(df)
              tags$div(drag="some name", class = "active content multiline",
                       apply(df, 1, function(row) {
                           n =names(row)
                           if (is.null(n)) {
                               return("")
                           }

                           type = row[which(n == "type")]
                           name = row[which(n == "name")]
                           eq = row[which(n == "eq")]

                           if (type == "symbol") {
                               obsList[[name]] <<- observeEvent(input[[name]], {
                                   cat("Symbol value ", name, " = ", input[[name]], "\n")
                                   idx <- which(workspace$data$type == "symbol" & workspace$data$name == name)
                                   subidx <- which(workspace$subs$idx == idx)
                                   if (length(subidx) > 0) {
                                      message("removing subs with index: ")
                                       print(subidx)
                                      workspace$subs <- workspace$subs[-subidx,]
                                   }
                                   workspace$subs <- rbind(workspace$subs, data.frame(idx = idx, value = as.numeric(input[[name]])))
                               })
                               #workspace$data[idx,]$sympy = paste0("subs(", name, ", ", input[[name]], ")")
                               tags$div(class="container", 
                                        tags$label(class="col-sm-2 col-form-label", 
                                                   paste0("$$", eq, "$$")), 
                                        textInput(name, NULL))
                           } else {
                               tags$div(paste("$$", eq, "$$"))
                           }
                       }))
          }))
  })
  
  output$equationList <- renderUI({
    #vars$frontMatter <- rmarkdown::yaml_front_matter(selectedProblem)
    div(id = "Available", style = "min-height: 250px;", 
        apply(workspace$equations, 1, makeElement))
  })
  
  # Dragula Functions
  output$dragula <- renderDragula({
    dragula(c("Available", "Model"), copyOnly = 'Available', removeOnSpill = TRUE)
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


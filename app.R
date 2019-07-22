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
                                   uiOutput("Model", style = "min-height: 200px;")
                            )
                     )
                   ),
                   fluidRow(
                       verbatimTextOutput("solution"),
                       verbatimTextOutput("workspace")
                   )
                 )
  )
)

# Define Server Function

server <- function(input, output) {
  selectedProblem <- file.path('problems', 'problem1.md')
  workspace = reactiveValues(data = initializeWorkspace(), 
                             model = c(), equations = c(),
                             subs = list())
  #subs = reactiveValues()
  
  obsList <- list()

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
  
  observeEvent(input$dragula, {
      workspace <- refreshWorkspace(workspace, dragulaValue(input$dragula)$Model)
  })
  
  output$solution <- eventReactive(workspace$subs, {
      message("subs changed")
      print(workspace$subs)
      solutions <- lapply(workspace$subs, function(subs) {
          if (nrow(subs) <= 0) {
              return("")
          }
          # wsnames <- workspace$subs$name[!is.na(workspace$subs$value)]
          # idx <- which(workspace$data$name %in% wsnames)
          # print(workspace$data[idx,]$sympy)
          # values <- workspace$subs$value[!is.na(workspace$subs$value)]
          # subs <- data.frame(symbol = workspace$data$sympy[idx], value = values)
          # vnames <- workspace$subs$name[!is.na(workspace$subs$value)]
          # workspace$data[workspace$data$name %in% vnames,]$show <- values
          solution <- workspaceSolve(workspace$data, subs)
          s <- solution[[1]]
          str(s)
          gid <- workspace$data[workspace$data$eq == s$eq,]$id
          #
          if (is.list(s$solution)) {
              partial <- paste0(names(s$solution)[[1]], " = ", as.character(s$solution[[1]]))
              workspace$data[workspace$data$eq == s$eq,]$show <- partial
              #message("solution list:")
              return(partial)
          } else {
              workspace$data[workspace$data$eq == s$eq,]$show <- NA
              idx <- which(workspace$data$id == gid & workspace$data$type == "symbol" & (is.na(workspace$data$show) | workspace$data$lock))
              message("replacing final value at index")
              print(idx)
              value <- as.numeric(py_str(s$solution))
              message(paste0("class of value: ", class(value), "..."))
              message(paste0("py_str: ", py_str(s$solution), "..."))
              #str(as.numeric(value))
              message("workspace:")
              print(workspace$data)
              if (nrow(workspace$data[idx,]) == length(value)) {
                  workspace$data[idx,]$show <- value
                  workspace$data[idx,]$lock <- TRUE
              }
              return(as.character(s$solution))
          }
          return(solution)
      })
      return(paste(solutions, collapse = "\n"))
      #print(solution[[1]])
      #return(py_to_r(solution[[1]]$solution[[1]]))
      #return(solution[[1]]$solution[[1]])
  })
  
  output$problemStatement <- renderUI({
    HTML(markdown::markdownToHTML(knit(selectedProblem, quiet = TRUE)))
  })
  
  observeEvent(workspace$data, {
      
  })
  
  output$ModelOld <- renderUI({
      if (nrow(workspace$data) == 0) {
          return("")
      }
      isolate(workspace$data) %>% 
          mutate(Group = group_indices(., id)) %>% 
          split(.$Group) ->
          chunks
      #message("chunks:")
      #print(chunks)
      list(withMathJax(),
           lapply(chunks, function(df) {
               #str(df)
               n = names(df)
               eqname <- df[df$type == "expression",]$name
               if (is.null(workspace$subs[[eqname]])) {
                   workspace$subs[[eqname]] <- data.frame(name = character(), symbol = character(), gid = integer(), value = numeric())
               }
               el <- tags$div(drag="some name", class = "active content multiline",
                        apply(df, 1, function(row) {
                            n =names(row)
                            if (is.null(n)) {
                                return("")
                            }
                            #print(row)
                            type = row[which(n == "type")]
                            name = row[which(n == "name")]
                            eq = row[which(n == "eq")]
                            show = row[which(n == "show")]
                            lock = trimws(row[which(n == "lock")])
                            
                            if (type == "symbol") {
                                if (FALSE && is.null(obsList[[name]])) {
                                    obsList[[name]] <<- observeEvent(input[[name]], {
                                        value = input[[name]]
                                        if (str_length(value) <= 0) {
                                            return()
                                        }
                                        #message(paste0("Symbol value ", name, " = ", value))
                                        value <- as.numeric(value)
                                        idx <- which(workspace$data$type == "symbol" & workspace$data$name == name)
                                        gid <- workspace$data[idx,]$id
                                        symname <- workspace$data[idx,]$sympy
                                        subidx <- which(workspace$subs[[eqname]]$name == name)
                                        if (length(subidx) > 0) {
                                            message(paste0("removing subs with index: [", paste(subidx, collapse =","), "]"))
                                            workspace$subs[[eqname]] <- workspace$subs[[eqname]][-subidx,]
                                        }
                                        nr <- data.frame(name = name, symbol = symname, gid = gid, value = value)
                                        #message("appending subs row")
                                        #print(nr)
                                        workspace$subs[[eqname]] <- rbind(workspace$subs[[eqname]], nr)
                                        #locked <- workspace$data$lock
                                        #if (any(locked)) {
                                        #    workspace$data[locked,]$lock <- FALSE
                                        #}
                                        #workspace$data$lock <- FALSE
                                        workspace$data[workspace$data$name == name,]$show <- value
                                    }, ignoreInit = TRUE)
                                }
                                #workspace$data[idx,]$sympy = paste0("subs(", name, ", ", input[[name]], ")")
                                if (lock == "TRUE") {
                                    message(paste0("destroying observer for ", name))
                                    obsList[[name]]$destroy()
                                    obsList[[name]] <- NULL
                                    el <- tags$div(show)
                                } else {
                                    el <- textInput(name, NULL, value = show)
                                }
                                tags$div(class="container", 
                                         tags$label(class="col-sm-2 col-form-label", 
                                                    paste0("$$", eq, "$$")), 
                                         el)
                            } else {
                                output_name <- paste0(name, "_partial")
                                if (!is.na(show)) {
                                    tags$div(paste("$$", show, "$$"))
                                } else {
                                    tags$div(paste("$$", eq, "$$"), textOutput(output_name, inline = TRUE))
                                }
                            }
                        }))
               el
               #workspace$subs[[eqname]] <- subs
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


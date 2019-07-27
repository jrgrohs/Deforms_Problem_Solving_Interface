symbolUI <- function(id, label) {
    ns <- NS(id)
    fluidRow(
        column(6, tags$label(paste0("$$", label, "$$"))),
        column(2, textInput(ns("value"), NULL)),
        column(4, verbatimTextOutput(ns("debug")))
    )
}

symbol_server <- function(input, output, session, value) {
    observeEvent(input$value, {
        message("input ", session$ns("value"), " has value ", input$value)
    })
    
    output$debug <- renderPrint(input$value)
    
    observeEvent(value, {
        #v <- value
        message(paste0("calculated value has changed to ", value))
        str(value)
        if (!is.na(value) && is.numeric(value)) {
            updateTextInput(session, "value", value = value())
        }
    })
    
    list(value = reactive({input$value}))
}
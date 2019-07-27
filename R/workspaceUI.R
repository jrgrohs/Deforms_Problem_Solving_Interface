source('R/workspace.R')
source('R/symbolUI.R')

workspaceUI <- function(id, label = "Main") {
    ns <- NS(id)
    tagList(
        #useShinyjs(),
        #useDragulajs(),
        h3(label),
        withMathJax(),
        wellPanel(id = ns("workspace")),
        #uiOutput(ns("solution")),
        verbatimTextOutput(ns("solution")),
        actionButton(ns("solve"), "Solve")#,
        #tags$hr(),
        #verbatimTextOutput(ns("preview")),
        #verbatimTextOutput(ns("handler"))
        #dragula(c("Available", ns("workspace")), id = ns("dragula"))
    )
}

workspaceModule <- function(input, output, session, drop, equationBank) {
    workspace <- reactiveValues(data = initializeWorkspace())
    handler <- reactiveVal(list())
    values <- reactiveValues()
    test <- reactiveVal("default")
    subs <- list()
    
    observeEvent(drop(), {
        workspace$data <- refreshWorkspace(workspace$data, drop())
        workspace$data %>%
            filter(status == "new") ->
            newItems
        
        if (nrow(newItems) <= 0) {
            return()
        }

        apply(newItems, 1, function(row) {
            message(paste0("Workspace: adding UI for new equation ", row[1], " with number ", row[2]))
            #appendSymbolUI(row)
            eqname <- row[1]
            eq <- first((equationBank %>% filter(name == eqname))$eq)
            symbols <- extractSymbols(eq)
            #message("new symbols")
            #print(symbols)
            insertUI(paste0("[drag='", eqname, "']"), "afterBegin", withMathJax())
            symbolInputs <- lapply(symbols, function(symbol) {
                uiID <- tex_to_symbol(paste0(symbol, "_", row[2]))
        
                message(paste0("insertingUI with ns name ", session$ns(uiID)))
                ui <- symbolUI(session$ns(uiID), symbol)
                insertUI(paste0("[drag='", eqname, "']"), "afterBegin", ui)
                
                handler_list <- isolate(handler())
                values[[uiID]] <- as.numeric(NA)
                new_handler <- callModule(symbol_server, uiID, values[[uiID]])
                handler_list <- c(handler_list, new_handler)
                names(handler_list)[length(handler_list)] <- session$ns(uiID)
                handler(handler_list)
                
                message(paste0("\tcreating reactiveVal values[[", uiID, "]]"))
                #new_value <- reactiveVal(as.numeric(NA))
                #values <- c(values, new_value)
                #names(values)[length(values)] <- uiID
                
                return(ui)
            })
            # insertUI(paste0("[drag='", eqname, "']"), "afterBegin", tags$div(class = "symbol_container", 
            #                                                                  withMathJax(), 
            #                                                                  symbolInputs))
            message("should now have a list of reactive values: ")
            #str(reactiveValuesToList(values))
            rows <- which(workspace$data$eqname == row[1] & workspace$data$eqnum == row[2])
            if (length(rows) > 0) {
                workspace$data[rows,]$status <- NA
            }
            
        })
    })
    
    solution <- eventReactive(input$solve, {
        message("solving the thing")
        symbols <- handler()
        symparts <- symbol_to_generic(names(symbols))
        lapply(symparts, function(parts) {
            #message("wtf is name?")
            #str(name)
            #message("and symbol_to_generic(name)")
            #str(symbol_to_generic(name))
            id <- paste(parts$ns, parts$symbol, sep = "-")
            value <- as.numeric(symbols[[id]]())        
            #print(paste0("input$", id, " (", parts$base, ")=", value))
            parts$value <- value
            return(parts)
        }) ->
            subs
        #message("subs")
        #str(subs)
        
        workspace$data %>%
            left_join(equationBank, by = c(eqname = "name")) ->
            ws
        
        solution = workspaceSolve(ws, subs)
        
        s <- solution[[1]]
        #str(s)
        return(s)
    })
    
    output$solution <- renderPrint(solution()$solution)
    
    # output$solution <- renderUI({
    #     s <- solution()
    #     if (is.list(s$solution)) {
    #         message("solution is a list")
    #         partial <- paste0(names(s$solution)[[1]], " = ", as.character(s$solution[[1]]))
    #         print(partial)
    #         tags$div(partial)
    #     } else {
    #         message("solution is not a list")
    #         #idx <- which(workspace$data$id == gid & workspace$data$type == "symbol" & (is.na(workspace$data$show) | workspace$data$lock))
    #         nasub <- first(subs[unlist(lapply(subs, function(x) is.na(x$value)))])
    #         inputname <- paste(nasub$ns, nasub$symbol, sep = "-")
    #         message(paste0("replacing final value at symbol name '", inputname, "'"))
    #         #print(idx)
    #         value <- as.numeric(py_str(s$solution))
    #         message(paste0("with value: ", value))
    #         message(paste0("updated reactiveVal values[[", nasub$symbol, "]]"))
    #         str(values[[nasub$symbol]])
    #         str(values)
    #         values[[nasub$symbol]] <- value
    #         tags$div(paste0(nasub$symbol, " = ", value))
    #         #updateTextInput(session, nasub$symbol, value = value)
    #     }
    #     
    # })

    output$preview <- renderPrint({
        workspace$data
    })
    
    
    output$handler <- renderPrint({
        #handler()
        lapply(handler(), function(handle) {
             handle()
        })
    })
    
    return(reactive(workspace$data))
}
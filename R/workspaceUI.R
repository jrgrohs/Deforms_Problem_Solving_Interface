library("shinyjs")
source('R/workspace.R')
source('R/symbolUI.R')

## This was a re-implementation of withMathJax()
queueMathJax <- "
shinyjs.queueMathJax = function(params) {
    console.log('queuing a MathJax render');
    if (window.MathJax) MathJax.Hub.Queue(['Typeset',MathJax.Hub]);
}
"
# but it faired no better in rendering MathJax in the dynamically inserted elements
# it could be that it is working proberly and window.MathJax is not valid once it is called... there is an error in the console about the page not having permission to load MathJax

workspaceUI <- function(id, label = "Main") {
    ns <- NS(id)
    tagList(
        useShinyjs(),
        extendShinyjs(text = queueMathJax), # for the re-implementation of withMathJax
        #useDragulajs(),
        h3(label),
        withMathJax(),
        wellPanel(id = ns("workspace")),
        uiOutput(ns("solution")),
        #verbatimTextOutput(ns("solution")),
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

        # iterate over the new items, accumulate reactive values for each symbolUI
        values <- purrr::reduce(newItems %>% split(seq(nrow(.))), function(acc, row) {
            eqname <- row$eqname
            #row = as.list(newItems[j,])
            #message("row is")
            #str(row)
            #message(paste0("Workspace: adding UI for new equation ", row[1], " with number ", row[2]))
            #eqname <- row[1]
            eq <- first((equationBank %>% filter(name == eqname))$eq)
            symbols <- extractSymbols(eq)
            #message("new symbols")
            #print(symbols)
            # iterate over the symboles in the new item
            values <- purrr::reduce(symbols, function(acc, symbol) {
                #symbolInputs <- lapply(symbols, function(symbol) {
                uiID <- tex_to_symbol(paste0(symbol, "_", row$eqnum))
                
                message(paste0("insertingUI with ns name ", session$ns(uiID)))
                ui <- symbolUI(session$ns(uiID), symbol)
                insertUI(paste0("[drag='", eqname, "']"), "afterBegin", ui)
                #insertUI(paste0("[drag='", eqname, "']"), "afterBegin", withMathJax())
                
                handler_list <- isolate(handler())
                message(paste0("creating reactive values[[", uiID, "]]"))
                acc[[uiID]] <- as.numeric(NA)
                new_handler <- callModule(symbol_server, uiID, acc[[uiID]])
                handler_list <- c(handler_list, new_handler)
                names(handler_list)[length(handler_list)] <- session$ns(uiID)
                handler(handler_list)
                
                return(acc);
            }, .init = values)
            message("list of reactives is")
            str(reactiveValuesToList(values))
            rows <- which(workspace$data$eqname == row[1] & workspace$data$eqnum == row[2])
            if (length(rows) > 0) {
                workspace$data[rows,]$status <- NA
            }            
            acc
        }, .init = values)
        
        message("reavtive values is now:")
        str(reactiveValuesToList(values))
        # why isn't this triggering a MathJax re-render?
        insertUI(paste0("#main-workspace"), "beforeEnd", withMathJax())
        #js$queueMathJax();
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

        workspace$data %>%
            left_join(equationBank, by = c(eqname = "name")) ->
            ws
        
        solution = workspaceSolve(ws, subs)
        
        s <- solution[[1]]
        #str(s)
        return(list(solution = s, subs = subs))
    })
    
    output$solution <- renderUI({
        s <- solution()$solution
        subs <- solution()$subs
        message("in renderUI, reactive values is")
        reactiveValuesToList(values) # why is the reactiveValues 'values' no longer a list?
        
        if (is.list(s$solution)) {
            message("solution is a list")
            partial <- paste0(names(s$solution)[[1]], " = ", as.character(s$solution[[1]]))
            print(partial)
            tags$div(partial)
        } else {
            message("solution is not a list")
            #idx <- which(workspace$data$id == gid & workspace$data$type == "symbol" & (is.na(workspace$data$show) | workspace$data$lock))

            nasub <- first(subs[unlist(lapply(subs, function(x) is.na(x$value)))])
            inputname <- paste(nasub$ns, nasub$symbol, sep = "-")
            message(paste0("replacing final value at symbol name '", inputname, "'"))
            #print(idx)
            value <- as.numeric(py_str(s$solution))
            message(paste0("with value: ", value))
            message(paste0("updated reactiveVal values[[", nasub$symbol, "]]"))
            #str(values[[nasub$symbol]])
            # if values was still a reactiveValues object then this should set the value of the named reactive, triggering a render of any dependants, namely the symbolUI 
            values[[nasub$symbol]] <- value
            tags$div(paste0(nasub$symbol, " = ", value))
            #updateTextInput(session, inputname, value = "poop on a stick")
        }

    })

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
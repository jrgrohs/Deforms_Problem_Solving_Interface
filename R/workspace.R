library(stringr)
library(purrr)
library(reticulate)

sympy = import("sympy")

extractSymbols <- function(eq) {
      symbols <- str_split(eq, "(\\s+|=)")
      symbols <- symbols[symbols != "="]
      lapply(symbols, function(s) { s = s[s != ""] }) # remove empty symbols
}

initializeWorkspace <- function() {
    # data.frame(
    #     id = integer(),
    #     eq = character(),
    #     name = character(),
    #     type = factor(levels = c("symbol", "expression")),
    #     sympy = character(),
    #     show = integer(),
    #     lock = logical(),
    #     stringsAsFactors = FALSE)
  data.frame(
    eqname = character(),
    eqnum = integer(),
    stringsAsFactors = FALSE)
}

tex_to_symbol <- function(tex) {
  str_replace_all(tex, "\\s+", " * ") %>%
    str_remove_all("[\\{\\}\\\\]") %>%
    str_replace_all("e\\^([-]?[.]?[0-9]+[.]?[0-9]*)", "exp(\\1)") %>% # e^x => exp(x)
    str_replace_all("\\^", "\\*\\*")
}

sympyify_str <- function(eq) {
  message(paste0("sympifying '", eq, "'"))
  components <- str_split(eq, "\\s*=\\s*")
  lapply(components, function(li) {
    if (length(li) > 2) {
      stop('Invalid express')
    }
    li = map_chr(li, tex_to_symbol)

    if (length(li) == 2) {
      paste0("Eq(", li[1], ", sympify(", li[2], "))")
    } else {
      li[1]
    }
  })
}

sympify <- function(eq) {
  components <- str_split(eq, "\\s*=\\s*")
  lapply(components, function(li) {
    if (length(li) > 2) {
      stop('Invalid express')
    }
    li = map_chr(li, tex_to_symbol)
    
    if (length(li) == 2) {
      sympy$Eq(sympy$sympify(li[1]), sympy$sympify(li[2]))
    } else {
      sympy$sympify(li[1])
    }
  })
}

refreshWorkspace <- function(workspace, items) {
  workspace$data %>% 
    group_by(eqname) %>% 
    summarize(n = n()) ->
    existing_equations
  
  message("existing equations")
  print(existing_equations)
  
  newItems <- items[! items %in% workspace$data$eqname]
  newRows <- lapply(newItems, function(item) {
    eqno <- (existing_equations[existing_equations$eqname == item,]$n)[1]
    if (is.na(eqno)) {
      eqno = 1
    } else {
      eqno = eqno + 1
    }
    newRow <- data.frame(eqname = item,
                         eqnum  = eqno)
    newRow
  })
  
  workspace$data <- rbind(workspace$data, newRows)
  message("New rows")
  print(newRows)
  message("New workspace data")
  print(workspace$data)
  workspace
}

oldRefreshWorkspace <- function(workspace, items) {
    message("refreshWorkspace")
    
    # get new workspace items, append a _ID onto them
    newItems <- items[! items %in% workspace$model]
    
    id <- nrow(workspace$data) + 1
    newRows <- lapply(newItems, function(item) {
        eq <- workspace$equations[which(workspace$equations$name == item), "eq"]
        name <- workspace$equations[which(workspace$equations$name == item), "name"]
        sympy <- sympyify_str(eq)[[1]]

        newRow <- data.frame(id = id, 
                             eq = eq, 
                             name = paste(name, id, sep = "_"), 
                             type = "expression",
                             sympy = sympy,
                             show = NA,
                             lock = FALSE,
                             stringsAsFactors = FALSE)
        symbols <- extractSymbols(newRow$eq)
        newSymbols <- lapply(symbols, function(sym) {
          message("sym:")
          print(sym)
            data.frame(id = id,
                       eq = sym,
                       name = paste(sym, id, sep = "_"),
                       type = "symbol",
                       sympy = tex_to_symbol(sym),
                       show = NA,
                       lock = FALSE,
                       stringsAsFactors = FALSE)
        })
        print(newSymbols)
        id <- id + 1
        rbind(do.call(rbind, newSymbols), newRow)
    })
    #message(paste0("binding ", length(newRows), " new rows:" ))
    #print(newRows)
    
    if (length(newRows) > 0) {
        newworkspace <- do.call(rbind, newRows)
        workspace$data <- rbind(workspace$data, newworkspace)
    }
    workspace$model <- items
    #list(model = items,  workspace = workspace)
    workspace
}

workspaceSolve <- function(ws, subs) {
  if (nrow(ws) <= 0) {
    return(NA)
  }
  message("workspaceSolve")
  print(subs)
  lsubs <- split(subs, seq(nrow(subs)))
  #print(lsubs)
  ws %>%
    filter(type == "expression") %>%
    apply(1, function(row) {
      message(paste0("solving ", row["eq"]))
      
      sym <- sympify(row[["eq"]])[[1]]
      print(sym)
      if (length(lsubs) <= 0) {
        return(sym)
      }
      sym <- purrr::reduce(lsubs, function(a, s) {
        message("row of subs")
        print(s)
        symbol <- s[,"symbol"]
        value <- s[,"value"]
        if (length(symbol) > 0 && str_length(symbol) > 0 && is.numeric(value)) {
          return(a$subs(sympy$Symbol(symbol), s[,"value"]))
        } else {
          return(a)
        }
      }, .init = sym)
      solution <- sympy$solve(sym)[[1]]
      return(list(eq = row[["eq"]], sym = sym, solution = solution))
    })
}

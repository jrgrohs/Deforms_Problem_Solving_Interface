library(stringr)
library(purrr)

extractSymbols <- function(eq) {
      symbols <- str_split(eq, "(\\s+|=)")
      symbols <- symbols[symbols != "="]
      lapply(symbols, function(s) { s = s[s != ""] }) # remove empty symbols
}

initializeWorkspace <- function() {
    data.frame(
        id = integer(),
        eq = character(),
        name = character(),
        type = factor(levels = c("symbol", "expression")),
        sympy = character(),
        parent = integer(),
        stringsAsFactors = FALSE)
}

tex_to_symbol <- function(tex) {
  str_replace_all(tex, "\\s+", " * ") %>%
    str_remove_all("[\\{\\}\\\\]") %>%
    str_replace_all("e\\^([-]?[.]?[0-9]+[.]?[0-9]*)", "exp(\\1)") %>% # e^x => exp(x)
    str_replace_all("\\^", "\\*\\*")
}

sympyify <- function(eq) {
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

refreshWorkspace <- function(workspace, items) {
    message("refreshWorkspace")
    
    newItems <- items[! items %in% workspace$model]
    
    id <- nrow(workspace$data) + 1
    newRows <- lapply(newItems, function(item) {
        eq <- workspace$equations[which(workspace$equations$name == item), "eq"]
        name <- workspace$equations[which(workspace$equations$name == item), "name"]
        sympy <- sympyify(eq)[[1]]

        newRow <- data.frame(id = id, 
                             eq = eq, 
                             name = paste(name, id, sep = "_"), 
                             type = "expression",
                             sympy = sympy,
                             parent = NA, # TODO: probably unused if we make the id the same for express and symbols
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
                       parent = id,
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

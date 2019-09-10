library(stringr)
library(purrr)
library(reticulate)

reticulate::use_python("/usr/bin/python")

sympy = import("sympy")

# extract dragula values for workspace module with id==workspace
workspaceValue <- function(x, workspace) {
  unlist(x[[paste0(workspace, "-workspace")]])
}

extractSymbols <- function(eq) {
      symbols <- str_split(eq, "(\\s+|=)")
      symbols <- symbols[symbols != "="]
      first(lapply(symbols, function(s) { s = s[s != ""] })) # remove empty symbols
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
    status = factor(c(), c("new", "remove", "reorder")),
    stringsAsFactors = FALSE)
}

tex_to_symbol <- function(tex) {
  str_replace_all(tex, "\\s+", " * ") %>%
    str_remove_all("[\\{\\}\\\\]") %>%
    str_replace_all("e\\^([-]?[.]?[0-9]+[.]?[0-9]*)", "exp(\\1)") %>% # e^x => exp(x)
    str_replace_all("\\^", "\\*\\*")
}

# TODO: rename, rewrite description
# Take a numbered, namespaced symbol such as 'main-L_1' and 
# return the generic symbol, in this example 'L'
symbol_to_generic <- function(x) {
  lapply(str_split(x, '-'), 
                function(x) {
                  symbol <- tail(x, 1)
                  ns <- head(x,1)
                  parts <- str_split(symbol, '_')[[1]]
                  base <- paste(head(parts, -1), collapse = '_')
                  eqnum <- tail(parts, 1)
                  data.frame(symbol = symbol, ns = ns, base = base, eqnum = eqnum, stringsAsFactors = FALSE)
  })
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

# Take a equation string and turn it into a sympy symbolic Eq object.
sympify <- function(eq, subs) {
  components <- str_split(eq, "\\s*=\\s*")
  lapply(components, function(li) {
    if (length(li) > 2) {
      stop('Invalid express')
    }
    li = vapply(map_chr(li, tex_to_symbol), function(exp) {
      purrr::reduce(subs, function(eq, sym) {
        first(str_replace(eq, sym$base, paste(sym$base, sym$eqnum, sep = "_")))
      }, .init = exp) 
    }, FUN.VALUE = "")
    
    if (length(li) == 2) {
      sympy$Eq(sympy$sympify(li[1]), sympy$sympify(li[2]))
    } else {
      sympy$sympify(li[1])
    }
  })
}

appendToWorkspace <- function(workspace, item) {
  eqnum <- nrow(workspace %>% filter(eqname == item)) + 1
  workspace <- bind_rows(workspace, 
                              data.frame(eqname = item, 
                                         eqnum = eqnum, 
                                         status = "new",
                                         stringsAsFactors = FALSE))
  # message("appendToWorkspace result of bind_rows")
  # str(workspace)
  workspace
}

# TODO: handle removal and reordering
refreshWorkspace <- function(workspace, items) {
  stopifnot("data.frame" %in% class(workspace))
  if (length(items) <= 0) {
    return(workspace)
  }
  
workspace %>% 
    group_by(eqname) %>% 
    summarize(n = n()) ->
    existing_equations
  
  as.data.frame(table(items), stringsAsFactors = FALSE) %>% 
    full_join(existing_equations, by = c(items = "eqname")) %>%
    mutate(new = if_else(!is.na(n), Freq - n, 1L)) %>%
    filter(new >= 1) ->
    newItemsdf

  if (nrow(newItemsdf) <= 0) {
    return(workspace)
  }
  
  expandedNewItems <- apply(newItemsdf, 1, function(row) { rep(row[1], row[4])})

  workspace <- purrr::reduce(expandedNewItems, appendToWorkspace, .init = workspace)
  workspace
}

workspaceSolve <- function(ws, subs) {
  if (nrow(ws) <= 0) {
    return(NA)
  }
  
  # gather rows of subs that are not NA and are numeric
  lsubs <- subs[sapply(subs, function(x) !is.na(x[,"value"]) && is.numeric(x[,"value"]))]
  
  ws %>%
    #filter(type == "expression ") %>%
    apply(1, function(row) {
      message(paste0("solving ", row["eq"]))
      
      sym <- sympify(row[["eq"]], subs)[[1]]
      print(sym)

      if (length(lsubs) <= 0) {
        # without any substitutions to apply, simply return the sympify object
        return(sym)
      }

      # with substitutions, apply each subsitution to the sympify object, accumulating the result
      sym <- purrr::reduce(lsubs, function(a, s) {
        symbol <- s[,"symbol"]
        value <- s[,"value"]
        if (length(symbol) > 0 && str_length(symbol) > 0) {
          res <- a$subs(sympy$Symbol(symbol), s[,"value"])
        } else {
          res <- a
        }
        return(res)
      }, .init = sym)
      message(paste0("sym for solver: ", sym))
      solution <- sympy$solve(sym)[[1]]
      return(list(eq = row[["eq"]], sym = sym, solution = solution))
    })
}

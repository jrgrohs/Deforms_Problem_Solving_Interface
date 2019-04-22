# this script creates an equation list


temp_change_formula.formula <- withMathJax("\\(\\delta_t = \\alpha \\Delta T L\\)")
temp_change_formula.num_vars <- 4
temp_change_formula.var1 <- withMathJax("\\(\\delta_t\\)")
temp_change_formula.var2 <- withMathJax("\\(\\alpha\\)")
temp_change_formula.var3 <- withMathJax("\\(\\Delta T\\)")
temp_change_formula.var4 <- withMathJax("\\(\\L\\)")
temp_change_formula <- list(temp_change_formula.formula,
                            temp_change_formula.num_vars,
                            temp_change_formula.var1,
                            temp_change_formula.var2,
                            temp_change_formula.var3,
                            temp_change_formula.var4)
temp_change_formula <- set_names(temp_change_formula,c("formula","num_vars","var1","var2","var3","var4"))

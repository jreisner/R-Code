# Source: 
#   Lanker, Cory Lee, 
#   "Local prediction and classification techniques for machine learning and data mining" (2015). 
#   Graduate Theses and Dissertations. 14404. http://lib.dr.iastate.edu/etd/14404

oneway_LLR <- function(data, var1) {
  data %>%
    group_by_(var1) %>%
    mutate(LLR = log(sum(is_attributed) + 1/2) - log(sum(is_attributed == 0) + 1/2)) %>%
    ungroup() %>%
    select(LLR) %>%
    return()
}

twoway_LLR <- function(data, var1, var2) {
  data %>%
    group_by_(var1, var2) %>%
    mutate(LLR = log(sum(is_attributed) + 1/2) - log(sum(is_attributed == 0) + 1/2)) %>% 
    ungroup() %>%
    unite_("grp", c(var1, var2), sep = ".") %>%
    select(grp, LLR) %>%
    return()
}

threeway_LLR <- function(data, var1, var2, var3) {
  data %>%
    group_by_(var1, var2, var3) %>%
    mutate(LLR = log(sum(is_attributed) + 1/2) - log(sum(is_attributed == 0) + 1/2)) %>% 
    ungroup() %>%
    unite_("grp", c(var1, var2, var3), sep = ".") %>%
    select(grp, LLR) %>%
    return()
}

fourway_LLR <- function(data, var1, var2, var3, var4) {
  data %>%
    group_by_(var1, var2, var3, var4) %>%
    mutate(LLR = log(sum(is_attributed) + 1/2) - log(sum(is_attributed == 0) + 1/2)) %>% 
    ungroup() %>%
    unite_("grp", c(var1, var2, var3, var4), sep = ".") %>%
    select(grp, LLR) %>%
    return()
}
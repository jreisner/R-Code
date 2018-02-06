# ------------------------------------------------------------------------------
# A function to compute the means, differences, confidence intervals, and 
# p-values from pairwise honest significant difference comparisons. Returns an 
# object of class data.frame. This essentially adds onto the TukeyHSD function.
#
# group_variable: vector of class numeric or factor that contains values 
#    indicating the group a particular observation belongs to
# number_variable: vector of class numeric or integer from which we conduct 
#    pairwise comparisons
# alpha: significance level. 0.05 by default.
# ------------------------------------------------------------------------------

pairwise_hsd <- function(group_variable, number_variable, alpha = 0.05) {
  model <- aov(number_variable ~ factor(group_variable))
  
  groups <- unique(group_variable)
  groups <- groups[order(groups)]
  
  combns <- t(combn(groups, 2))
  results <- data.frame(V1 = combns[, 2],
                        V2 = combns[, 1])
  
  mean_num_var <- function(x) {
    mean(number_variable[group_variable == x], na.rm = TRUE)
  }
  
  results$V1_mean <- unlist(lapply(results$V1, mean_num_var))
  results$V2_mean <- unlist(lapply(results$V2, mean_num_var))
  
  nr <- nrow(results)
  hsd <- unlist(TukeyHSD(model, conf.level = 1 - alpha))
  
  results$diff <- hsd[1:nr]
  results$lower_ci <- hsd[(nr + 1):(2 * nr)]
  results$upper_ci <- hsd[(2 * nr + 1):(3 * nr)]
  results$p_value <- hsd[(3 * nr + 1):(4 * nr)]
  
  results <- results[, c(1, 2, 8, 3:7)]
  colnames(results)[c(1:2, 4:5)] <- c("Group1", "Group2", 
                                      "Group1Mean", "Group2Mean")
  results[, 3] <- round(results[, 3], 4)
  results[, 4:8] <- round(results[, 4:8], 3)
  
  results <- results[order(results$p_value),]
  
  return(results)
  
}

# Example ----------------------------------------------------------------------
# compare levels of ozone between months
data("airquality")
pairwise_hsd(airquality$Month, airquality$Ozone, alpha = 0.1)

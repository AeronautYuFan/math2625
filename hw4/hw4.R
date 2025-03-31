library(DescTools)
source('ORFun.R')

# formatting stuff (add latex font)
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = 'ComputerModern', regular = 'cmunrm.ttf') #for consistent formatting in LaTex
showtext_auto()
par(family = 'ComputerModern')
# end formatting stuff


whz = read.table('whz.txt', header = T)
bySex = with(whz, table(stress, wheeze, sex) )

BreslowDayTest(bySex)
mantelhaen.test(bySex, correct = F)






prop_ones <- colMeans(whz)  # 1s
prop_zeros <- 1 - prop_ones  # 0s

prop_matrix <- rbind(prop_zeros, prop_ones)

bar_positions <- barplot(prop_matrix, col = c("lightblue", "lightpink"), 
                         names.arg = names(whz), 
                         main = "", 
                         xlab = "Proportion", 
                         horiz = TRUE,  # horizontal chart
                         xlim = c(0, 1))

custom_labels <- c("No Wheezing", 
                   "No Increased Bedtime Stress", 
                   "Male", 
                   "Non-smoking Mother",
                   "Atopic Child") 
# these labels are so people know what the blue and red mean



text(y = bar_positions, x = prop_zeros / 2,
     labels = custom_labels, col = "black", cex = 1.2, font = 2)



oddsratio.matched(4, 12)


oddsratio.matched <- function(n21, n12, conf_level = 0.95) {
  or_value <- n21 / n12
  
  # standard error for CI
  se_log_or <- sqrt(1/n21 + 1/n12)
  
  # z score for CI
  z_score <- qnorm(1 - (1 - conf_level) / 2)
  
  log_or <- log(or_value)
  lower_log_or <- log_or - z_score * se_log_or
  upper_log_or <- log_or + z_score * se_log_or
  
  # untransform ci
  lower_or <- exp(lower_log_or)
  upper_or <- exp(upper_log_or)
  
  # code for printing
  or_value <- round(or_value, 7)
  lower_or <- round(lower_or, 7)
  upper_or <- round(upper_or, 7)
  
  output_text <- paste(
    "Matched-pairs odds ratio (", 
    conf_level * 100, "% confidence interval)\n", 
    "OR: ", or_value, "\n", 
    "Lower    Upper\n", 
    lower_or, "    ", upper_or,
    sep = ""
  )
  
  # print
  cat(output_text)
}

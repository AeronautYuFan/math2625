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

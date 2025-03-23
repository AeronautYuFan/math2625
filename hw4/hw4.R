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





# Calculate proportions of 1s and 0s for each variable
prop_ones <- colMeans(whz)  # Proportion of 1s
prop_zeros <- 1 - prop_ones  # Proportion of 0s

# Create a matrix for stacked bar plot (100% scale)
prop_matrix <- rbind(prop_zeros, prop_ones)

# Create horizontal stacked bar chart (100% scale)
bar_positions <- barplot(prop_matrix, col = c("lightblue", "lightpink"), 
                         names.arg = names(whz), 
                         main = "", 
                         xlab = "Proportion", 
                         horiz = TRUE,  # Rotate bars horizontally
                         xlim = c(0, 1))  # Ensures bars are scaled from 0 to 1

# Define custom labels (a, b, c, d, e)
custom_labels <- c("No Wheezing", 
                   "No Increased Bedtime Stress", 
                   "Male", 
                   "Non-smoking Mother",
                   "Atopic Child")

# Add custom text labels to the blue section only
text(y = bar_positions, x = prop_zeros / 2,  # Position in the middle of blue section
     labels = custom_labels, col = "black", cex = 1.2, font = 2)  # Bold text

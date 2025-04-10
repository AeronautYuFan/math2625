## case 1 ##
library('survival')
# libraries and code for fancy graph formatting
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = 'ComputerModern', regular = 'cmunrm.ttf') #for consistent formatting in LaTex
showtext_auto()
par(family = 'ComputerModern')
# end formatting stuff
## case 1 ##

blad <- read.csv('bladder.txt', header=TRUE, sep="")

## first recurrence by treatment group

recur  <- survfit(Surv(time, status1) ~ treatment, data = blad)
recur 
recur$strata

plot(recur, col = c('purple', 'green3', 'blue'), lty = c(1,2,3), 
     main = 'First Recurrence by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple', 'green3', 'blue'), lty = c(1,2,3), lwd = 2)



survdiff(Surv(time, status1) ~ treatment, data = blad, rho = 1) 

table(blad$treatment, blad$status1)
chisq.test(table(blad$treatment, blad$status1))

plot(recur, col = c('purple', 'green3', 'blue'), lty = c(1,2,3), 
     main = 'Cumulative Hazard First Recurrence by Treatment', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend("bottomright", legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple', 'green3', 'blue'), lty = c(1,2,3), lwd = 2)


## death  by treatment group

death  <- survfit(Surv(time, status2) ~ treatment, data = blad)
death 
death$strata

plot(death, col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2,3), 
     main = 'Death by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomleft", legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status2) ~ treatment, data = blad, rho = 1) 
table(blad$treatment, blad$status2)
chisq.test(table(blad$treatment, blad$status2))

plot(death, col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2), 
     main = 'Cumulative Hazard of Death by Treatment', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend("topleft", legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2,3), lwd = 2)



## first recurrence by number of tumors
number_recur  <- survfit(Surv(time, status1) ~ number, data = blad)
number_recur
number_recur$strata

plot(number_recur, col = c('red', 'orange', 'yellow3'), lty = c(1,2,3), 
     main = 'Recurrence by Tumor Number', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('One', 'Two to Three', 'Four or More'), 
       col = c('orange', 'yellow3', 'red'), lty = c(3,1,2), lwd = 2)

survdiff(Surv(time, status1) ~ number, data = blad, rho = 1)
table(blad$number, blad$status1)
chisq.test(table(blad$number, blad$status1))



## first recurrence by size of tumors 
size_recur  <- survfit(Surv(time, status1) ~ size, data = blad)
size_recur 
size_recur$strata

plot(size_recur, col = c('red', 'forestgreen', 'blue'), lty = c(1,2,3), 
     main = 'Bladder Survival for Size', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('1cm', '2cm to 3cm', '4cm or Larger'), 
       col = c('red', 'forestgreen', 'blue'), lty = c(1,2,3), lwd = 2)


survdiff(Surv(time, status1) ~ size, data = blad, rho = 1)
table(blad$size, blad$status1)
chisq.test(table(blad$size, blad$status1))



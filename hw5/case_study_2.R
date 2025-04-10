## case 2 ##
library('survival')
# libraries and code for fancy graph formatting
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = 'ComputerModern', regular = 'cmunrm.ttf') #for consistent formatting in LaTex
showtext_auto()
par(family = 'ComputerModern')
# end formatting stuff

firstr <- read.csv('cgd1.txt', header=TRUE, sep="")
secondr <- read.csv('cgd2.txt', header=TRUE, sep="")

## First infection by treatment group 
first_treat  <- survfit(Surv(time, status) ~ treatment, data = firstr)
first_treat 
first_treat$strata

plot(first_treat, col = c('red', 'blue'), lty = c(1,2,3), 
     main = 'First Infection by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomleft", legend = c('Placebo', 'Treatment'), 
       col = c('red', 'blue'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status) ~ treatment, data = firstr, rho = 1)

table(firstr$treatment, firstr$status)
chisq.test(table(firstr$treatment, firstr$status))

plot(first_treat, col = c('red', 'blue'), lty = c(1,2,3), 
     main = 'Cumulative Hazard \nof First Infection by Treatment', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend("topleft", legend = c('Placebo', 'Treatment'), 
       col = c('red', 'blue'), lty = c(1,2,3), lwd = 2)



## Second infection by treatment group 
second_treat  <- survfit(Surv(time, status) ~ treatment, data = secondr)
second_treat 
second_treat$strata


plot(second_treat, col = c('red3', 'blue3'), lty = c(1,2,3), 
     main = 'Second Infection by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('Placebo', 'Treatment'), 
       col = c('red3', 'blue3'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status) ~ treatment, data = secondr, rho = 1)
table(secondr$treatment, secondr$status)
chisq.test(table(secondr$treatment, secondr$status))


plot(second_treat, col = c('red3', 'blue3'), lty = c(1,2,3), 
     main = 'Cumulative Hazard \nof Second Infection by Treatment', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend("bottomright", legend = c('Placebo', 'Treatment'), 
       col = c('red3', 'blue3'), lty = c(1,2,3), lwd = 2)



## First recurrence by inherit group 
first_inherit  <- survfit(Surv(time, status) ~ inherit, data = firstr)
first_inherit 
first_inherit$strata

plot(first_inherit, col = c('orange', 'purple'), lty = c(1,2), 
     main = 'First Recurrence by Inherit', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('Autosomal', 'X-linked'), 
       col = c('orange', 'purple'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status) ~ inherit, data = firstr, rho = 1)
table(firstr$inherit, firstr$status)
chisq.test(table(firstr$inherit, firstr$status))


## First recurrence by prophylactic use group 
first_prop  <- survfit(Surv(time, status) ~ propylac, data = firstr)
first_prop
first_prop$strata

plot(first_prop, col = c('yellow2', 'green3'), lty = c(1,2,3), 
     main = 'First Recurrence by Prophylactic Use', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('No Use', 'Yes Use'), 
       col = c('yellow2', 'green3'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status) ~ propylac, data = firstr, rho = 1)
table(firstr$propylac, firstr$status)
chisq.test(table(firstr$propylac, firstr$status))


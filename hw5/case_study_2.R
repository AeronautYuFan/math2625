## case 2 ##
library(survival)
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
     main = '', xlab = 'Time (days)', 
     ylab = 'Probability of Being Free of First Infection', lwd = 2)
legend("bottomleft", legend = c('Placebo', 'Treatment'), 
       col = c('red', 'blue'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status) ~ treatment, data = firstr, rho = 1)

table(firstr$treatment, firstr$status)
chisq.test(table(firstr$treatment, firstr$status))

#### cumulative hazard
plot(first_treat, col = c('red', 'blue'), lty = c(1,2,3), 
     main = 'Cumulative Hazard \nof First Infection by Treatment', xlab = 'Time (days)',
     ylab = 'Probability of Being\nFree of First Infection',lwd = 2,
     fun = 'cumhaz')
legend("topleft", legend = c('Placebo', 'Treatment'), 
       col = c('red', 'blue'), lty = c(1,2,3), lwd = 2)

#####
par(cex = 1.5)

placebo = subset(firstr, treatment == 'placebo')
surv_plac = survfit(Surv(time, status) ~ 1, data = placebo)

plot(surv_plac, 
     col = 'red', lty = 1, lwd = 2, main = '', xlab = 'Time (days)', 
     ylab = 'Probability of Being Free of First Infection')
legend('topright', legend = c('Placebo', '95% CI'),
       col = 'red', lty = c(1,2), lwd = 2)

#treatment
pyrid = subset(firstr, treatment == 'rIFN-g')
surv_rIFNg = survfit(Surv(time, status) ~ 1, data = pyrid)

plot(surv_rIFNg, 
     col = 'blue', lty = 1, lwd = 2, main = '', xlab = 'Time (days)', 
     ylab = 'Probability of Being Free of First Infection')
legend('bottomleft', legend = c('rIFN-g', '95% CI'),
       col = 'blue', lty = c(1,2), lwd = 2)


######## end of individual graphs section



## Second infection by treatment group 
par(cex = 1)
second_treat  <- survfit(Surv(time, status) ~ treatment, data = secondr)
second_treat 
second_treat$strata


plot(second_treat, col = c('red3', 'blue3'), lty = c(1,2,3), 
     main = '', xlab = 'Time (days)',
     ylab = 'Probability of Being Free of Second Infection', lwd = 2)
legend("topright", legend = c('Placebo', 'Treatment'), 
       col = c('red3', 'blue3'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status) ~ treatment, data = secondr, rho = 1)
table(secondr$treatment, secondr$status)
chisq.test(table(secondr$treatment, secondr$status))


## cumulative hazard function
plot(second_treat, col = c('red3', 'blue3'), lty = c(1,2,3), 
     main = 'Cumulative Hazard \nof Second Infection by Treatment', xlab = 'Time (days)',
     ylab = 'Probability of Being\nFree of Second Infection',lwd = 2,
     fun = 'cumhaz')
legend("bottomright", legend = c('Placebo', 'Treatment'), 
       col = c('red3', 'blue3'), lty = c(1,2,3), lwd = 2)


## First recurrence by inherit group 
first_inherit  <- survfit(Surv(time, status) ~ inherit, data = firstr)
first_inherit 
first_inherit$strata

#### confounders: genetics and prop. use
plot(first_inherit, col = c('orange', 'purple'), lty = c(1,2), 
     main = '', xlab = 'Time (days)',
     ylab = 'Probability of Being Free of First Infection', lwd = 2)
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
     main = 'First Recurrence by Prophylactic Use', xlab = 'Time (days)',
     ylab = 'Probability of Being Free of First Infection', lwd = 2)
legend("bottomright", legend = c('No Use of Prophylactics', 'Usage of Prophylactics'), 
       col = c('yellow2', 'green3'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status) ~ propylac, data = firstr, rho = 1)
table(firstr$propylac, firstr$status)
chisq.test(table(firstr$propylac, firstr$status))


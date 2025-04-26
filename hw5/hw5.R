## case 1 ##
library(survival)
# libraries and code for fancy graph formatting
library(showtext)
library(DescTools)
library(Rfit)
font_add(family = 'ComputerModern', regular = 'cmunrm.ttf') #for consistent formatting in LaTex
showtext_auto()
par(family = 'ComputerModern')
# end formatting stuff
## case 1 ##

blad <- read.csv('bladder.txt', header=TRUE, sep='')

## Part 1. first recurrence of cancer by treatment group

recur  <- survfit(Surv(time, status1) ~ treatment, data = blad)

# Figure 1. Kaplan-Meier Estimate of Time to Recurrence in Subjects by Treatment
plot(recur, col = c('purple', 'green3', 'blue'), lty = c(1,2,3), 
     main = '', xlab = 'Time (months)',
     ylab = 'Probability of Being Free of Bladder Cancer', lwd = 2)
legend('topright', legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple', 'green3', 'blue'), lty = c(1,2,3), lwd = 2)

# wilcoxon test for treatment
survdiff(Surv(time, status1) ~ treatment, data = blad, rho = 1) 

########## More Graphs ====================================
par(cex = 1.5)

placebo = subset(blad, treatment == 'placebo')
surv_plac = survfit(Surv(time, status1) ~ 1, data = placebo)

plot(surv_plac, 
     col = 'purple', lty = 1, lwd = 2, main = '', xlab = 'Time (months)', 
     ylab = 'Probability of Being\nFree of Bladder Cancer', mark.time = T)
legend('topright', legend = c('Placebo', '95% CI'),
       col = 'purple', lty = c(1,2), lwd = 2)

#pyridoxine
pyrid = subset(blad, treatment == 'pyridoxine')
surv_pyrid = survfit(Surv(time, status1) ~ 1, data = pyrid)

plot(surv_pyrid, 
     col = 'green3', lty = 1, lwd = 2, main = '', xlab = 'Time (months)', 
     ylab = 'Probability of Being\nFree of Bladder Cancer')
legend('topright', legend = c('Pyridoxine', '95% CI'),
       col = 'green3', lty = c(1,2), lwd = 2)

#thiotepa
#genuinely have no idea how the spellings for these drugs were made
thi = subset(blad, treatment == 'thiotepa')
surv_thi = survfit(Surv(time, status1) ~ 1, data = thi)

plot(surv_thi, 
     col = 'blue', lty = 1, lwd = 2, main = '', xlab = 'Time (months)', 
     ylab = 'Probability of Being\nFree of Bladder Cancer')
legend('topright', legend = c('Thiotepa', '95% CI'),
       col = 'blue', lty = c(1,2), lwd = 2)

######## end of individual graphs section


# chi square association test to see if censoring rates differed
table(blad$treatment, blad$status1)
chisq.test(table(blad$treatment, blad$status1))
# censoring rates did not occur

########## cumulative hazard functions (might not include)
plot(recur, col = c('darkorchid4', 'darkgreen', 'darkblue'), lty = c(1,2,3), 
     main = 'Cumulative Hazard First Recurrence by Treatment', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend('bottomright', legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('darkorchid4', 'darkgreen', 'darkblue'), lty = c(1,2,3), lwd = 2)



## survival analysis of death by treatment group
death  <- survfit(Surv(time, status2) ~ treatment, data = blad)
death
death$strata

plot(death, col = c('darkorchid4', 'darkgreen', 'navy'), lty = c(1,2,3), 
     main = '', xlab = 'Time (months)', lwd = 2,
     ylab = 'Probability of Survival')
legend('bottomleft', legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('darkorchid4', 'darkgreen', 'navy'), lty = c(1,2,3), lwd = 2)

survdiff(Surv(time, status2) ~ treatment, data = blad, rho = 1) 
table(blad$treatment, blad$status2)
chisq.test(table(blad$treatment, blad$status2))


######################## cumulative hazard ##########################
## https://r-charts.com/colors/

plot(death, col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2), 
     main = 'Cumulative Hazard of Death by Treatment', xlab = 'Time (months)', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend('topleft', legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2,3), lwd = 2)


################## interactors/confounders
############### first recurrence by number of tumors
par(cex = 1.25)
number_recur  <- survfit(Surv(time, status1) ~ number, data = blad)
number_recur
number_recur$strata

#### recurrence by tumor number
plot(number_recur, col = c('red', 'orange', 'yellow3'), lty = c(1,2,3), 
     main = '', xlab = 'Time (months)', lwd = 2,
     ylab = 'Probability of Being Free of Bladder Cancer')
legend('topright', legend = c('One', 'Two to Three', 'Four or More'), 
       col = c('orange', 'yellow3', 'red'), lwd = 2)

survdiff(Surv(time, status1) ~ number, data = blad, rho = 1)
table(blad$number, blad$status1)
chisq.test(table(blad$number, blad$status1))



## first recurrence by size of tumors 
size_recur  <- survfit(Surv(time, status1) ~ size, data = blad)
size_recur 
size_recur$strata

plot(size_recur, col = c('darkred', 'forestgreen', 'darkblue'), lty = c(1,2,3), 
     main = '', xlab = 'Time (months)', lwd = 2,
     ylab = 'Probability of Being Free of Bladder Cancer')
legend('topright', legend = c('1 cm', '2-3 cm', '>4 cm '), 
       col = c('darkred', 'forestgreen', 'darkblue'), , lwd = 2)


survdiff(Surv(time, status1) ~ size, data = blad, rho = 1)
table(blad$size, blad$status1)
chisq.test(table(blad$size, blad$status1))



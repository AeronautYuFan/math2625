## case 1 ##
library('survival')

blad <- read.csv('bladder.txt', header=TRUE, sep="")

recur  <- survfit(Surv(time, status1) ~ treatment, data = blad)
recur 
recur$strata

##### Survivor Function #####
plot(recur, col = c('purple', 'green3', 'blue'), lty = c(1,2,3), 
     main = 'First Recurrence by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple', 'green3', 'blue'), lty = c(1,2,3), lwd = 2)



death  <- survfit(Surv(time, status2) ~ treatment, data = blad)
death 
death$strata
##### Survivor Function #####
plot(death, col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2,3), 
     main = 'Death by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple3', 'forestgreen', 'blue3'), lty = c(1,2,3), lwd = 2)



##### xxxCumulative Hazard Function #####
plot(model, col = c('purple', 'forestgreen', 'blue'), lty = c(1,2), 
     main = 'Bladder Study\nK-M Estimator', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend("topright", legend = c('Placebo', 'Pyridoxine', 'Thiotepa'), 
       col = c('purple', 'forestgreen', 'blue'), lty = c(1,2,3), lwd = 2)



## number of initial tumors ##
number_recur  <- survfit(Surv(time, status1) ~ number, data = blad)
number_recur
number_recur$strata

##### Survivor Function #####
plot(number_recur, col = c('red', 'orange', 'yellow2'), lty = c(1,2,3), 
     main = 'Recurrence by Tumor Number', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('One', 'Two to Three', 'Four or More'), 
       col = c('orange', 'yellow2', 'red'), lty = c(3,1,2), lwd = 2)


number_death  <- survfit(Surv(time, status2) ~ number, data = blad)
number_death 
number_death $strata

##### Survivor Function #####
plot(number_death, col = c('red3', 'orange2', 'yellow3'), lty = c(1,2,3), 
     main = 'Survival by Tumor Number', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('One', 'Two to Three', 'Four or More'), 
       col = c('orange2', 'yellow3', 'red3'), lty = c(3,1,2), lwd = 2)


##### Cumulative Hazard Function #####

plot(model, col = c('purple', 'forestgreen', 'blue'), lty = c(1,2), 
     main = 'Bladder for # \nK-M Estimator', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend("topright", legend = c('One', 'Two to Three', 'Four or More'), 
       col = c('forestgreen', 'blue', 'purple'), lty = c(3,1,2), lwd = 2)


## tumor size ## 
size_recur  <- survfit(Surv(time, status1) ~ size, data = blad)
size_recur 
size_recur$strata

plot(size_recur, col = c('red', 'forestgreen', 'blue'), lty = c(1,2,3), 
     main = 'Bladder Survival for Size', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('1cm', '2cm to 3cm', '4cm or Larger'), 
       col = c('red', 'forestgreen', 'blue'), lty = c(3,1,2), lwd = 2)

size_death  <- survfit(Surv(time, status2) ~ size, data = blad)
size_death 
size_death$strata

plot(size_death, col = c('red2', 'forestgreen', 'blue'), lty = c(1,2,3), 
     main = 'Bladder Survival for Size', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("topright", legend = c('1cm', '2cm to 3cm', '4cm or Larger'), 
       col = c('red2', 'forestgreen', 'blue'), lty = c(3,1,2), lwd = 2)

##### Cumulative Hazard Function #####

plot(model, col = c('purple', 'forestgreen', 'blue'), lty = c(1,2), 
     main = 'Bladder for Size nK-M Estimator', xlab = 't', lwd = 2,
     fun = 'cumhaz')
title(ylab = expression(hat(H)(t)), line = 2.5)
legend("topright", legend = c('1cm', '2cm to 3cm', '4cm or Larger'), 
       col = c('purple', 'forestgreen', 'blue'), lty = c(3,1,2), lwd = 2)


## case 2 ## 

firstr <- read.csv('cgd1.txt', header=TRUE, sep="")
secondr <- read.csv('cgd2.txt', header=TRUE, sep="")


first_treat  <- survfit(Surv(time, status) ~ treatment, data = firstr)
first_treat 
first_treat$strata

##### Survivor Function #####
plot(first_treat, col = c('red', 'blue'), lty = c(1,2,3), 
     main = 'First Recurrence by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('Placebo', 'Treatment'), 
       col = c('red', 'blue'), lty = c(1,2,3), lwd = 2)



second_treat  <- survfit(Surv(time, status) ~ treatment, data = secondr)
second_treat 
second_treat$strata

##### Survivor Function #####
plot(second_treat, col = c('red3', 'blue3'), lty = c(1,2,3), 
     main = 'Second Recurrence by Treatment', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('Placebo', 'Treatment'), 
       col = c('red3', 'blue3'), lty = c(1,2,3), lwd = 2)



## inherit ## 

first_inherit  <- survfit(Surv(time, status) ~ inherit, data = firstr)
first_inherit 
first_inherit$strata

##### Survivor Function #####
plot(first_inherit, col = c('orange', 'purple'), lty = c(1,2,3), 
     main = 'First Recurrence by Inherit', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('Autosomal', 'X-linked'), 
       col = c('orange', 'purple'), lty = c(1,2,3), lwd = 2)


second_inherit  <- survfit(Surv(time, status) ~ inherit, data = secondr)
second_inherit  
second_inherit$strata

##### Survivor Function #####
plot(second_inherit , col = c('darkorange', 'purple3'), lty = c(1,2,3), 
     main = 'Second Recurrence by Inherit', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('Autosomal', 'X-linked'), 
       col = c('darkorange', 'purple3'), lty = c(1,2,3), lwd = 2)


## Prophylactic usage 
first_prop  <- survfit(Surv(time, status) ~ propylac, data = firstr)
first_prop
first_prop$strata

##### Survivor Function #####
plot(first_prop, col = c('yellow2', 'green3'), lty = c(1,2,3), 
     main = 'First Recurrence by Prophylactic Use', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('No Use', 'Yes Use'), 
       col = c('yellow2', 'green3'), lty = c(1,2,3), lwd = 2)


second_prop  <- survfit(Surv(time, status) ~ propylac, data = secondr)
second_prop   
second_prop$strata

##### Survivor Function #####
plot(second_prop, col = c('yellow3', 'green4'), lty = c(1,2,3), 
     main = 'Second Recurrence by Prophylactic Use', xlab = 't', lwd = 2)
title(ylab = expression(hat(S)(t)), line = 2.5)
legend("bottomright", legend = c('No Use', 'Yes Use'), 
       col = c('yellow3', 'green4'), lty = c(1,2,3), lwd = 2)


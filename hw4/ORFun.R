#### Odds Ratio Functions ####
# Primarily for Note Set II #

#### Local Odds Ratio ####
lor		<- function(tab, conf.level = NULL){
  I			<- nrow(tab)
  J			<- ncol(tab)
  thetaij		<- vector(length = (I-1)*(J-1))
  outRownames	<- vector(length = I-1)
  outColnames	<- vector(length = J-1)
  if(!is.null(conf.level)){
    a   <- 1-conf.level
    if(a < 0 | a > 1){
      stop('conf.level must be between 0 and 1')
    }
    zs			<- qnorm(1-a/2)
    ciij		<- matrix(NA, nrow = (I-1)*(J-1), ncol = 2)
  }
  count		<- 1
  for(i in 1:(I-1)){
    outRownames[i]	<- paste(rownames(tab)[i:(i+1)], collapse = " vs ")
    for(j in 1:(J-1)){
      thetaij[count]	<- tab[i,j]*tab[i+1,j+1]/(tab[i,j+1]*tab[i+1,j])
      if(!is.null(conf.level)){
        ciij[count, 1]	<- exp(log(thetaij[count]) - zs*sqrt(1/tab[i,j] + 1/tab[i+1,j+1] + 1/tab[i,j+1] + 1/tab[i+1,j]))
        ciij[count, 2]	<- exp(log(thetaij[count]) + zs*sqrt(1/tab[i,j] + 1/tab[i+1,j+1] + 1/tab[i,j+1] + 1/tab[i+1,j]))
      }
      count			<- count + 1
    }
  }
  for(j in 1:(J-1)){
    outColnames[j]	<- paste(colnames(tab)[j:(j+1)], collapse = " vs ")
  }
  if(is.null(conf.level)){
    out			<- matrix(thetaij, nrow = I-1, byrow = TRUE)
    rownames(out)	<- outRownames
    colnames(out)	<- outColnames
  } else{
    theMat          <- matrix(thetaij, ncol = I-1, byrow = TRUE)
    out             <- cbind(theMat, ciij)
    rownames(out)   <- outColnames
    colnames(out)   <- c('Est.', 'Lower', 'Upper')
  }
  
  return(out)
}


#### Reference Odds Ratio ####
ror		<- function(tab){
  I			<- nrow(tab)
  J			<- ncol(tab)
  alphaij		<- vector(length = (I-1)*(J-1))
  outRownames	<- vector(length = I-1)
  outColnames	<- vector(length = J-1)
  count	<- 1
  for(i in 1:(I-1)){
    outRownames[i]	<- paste(c(rownames(tab)[i], rownames(tab)[I]), collapse = " vs ")
    for(j in 1:(J-1)){
      alphaij[count]	<- tab[i,j]*tab[I,J]/(tab[I,j]*tab[i,J])
      count			<- count + 1
    }
  }
  for(j in 1:(J-1)){
    outColnames[j]	<- paste(c(colnames(tab)[j], colnames(tab)[J]), collapse = " vs ")
  }
  out				<- matrix(alphaij, nrow = I-1, byrow = TRUE)
  rownames(out)	<- outRownames
  colnames(out)	<- outColnames
  return(out)
}



#### Conditional Odds Ratio ####
condOR		<- function(tab, conf.level = NULL){
  K		              <- dim(tab)[3]
  thetak	          <- matrix(0, nrow = K, ncol = 1, dimnames = list(Strata = c(1:K)))
  colnames(thetak)	<- 'OR Estimate'
  if(!is.null(conf.level)){
    a   <- 1-conf.level
    if(a < 0 | a > 1){
      stop('conf.level must be between 0 and 1')
    }
    zs			<- qnorm(1-a/2)
    cik		  <- matrix(NA, nrow = K, ncol = 2)
  }
  for(k in 1:K){
    thetak[k,1]	<- tab[1,1,k]*tab[2,2,k]/(tab[1,2,k]*tab[2,1,k])
    if(!is.null(conf.level)){
      cik[k,1]   <- thetak[k,1] - zs*sqrt(1/tab[1,1,k] + 1/tab[2,2,k] + 1/tab[1,2,k] + 1/tab[2,1,k])
      cik[k,2]   <- thetak[k,1] + zs*sqrt(1/tab[1,1,k] + 1/tab[2,2,k] + 1/tab[1,2,k] + 1/tab[2,1,k])
    }
  }
  if(is.null(conf.level)){
    out	<- list(condORs = thetak, nstrata = K, conf.level = conf.level)
  } else{
    colnames(cik) <- c('Lower', 'Upper')
    out	<- list(condORs = thetak, nstrata = K, conf.level = conf.level, cik = cik)
  }
  class(out)	<- 'condOR'
  return(out)
}

print.condOR	<- function(x){
  if(is.null(x$conf.level)){
    cat('Conditional odds ratios for the', x$nstrata, 'strata\n')
    print(round(x$condORs,4))
  } else{
    cat('Conditional odds ratios for the', x$nstrata, 'strata,\n')
    cat('with CI at', paste(round(100*x$conf.level), '% level\n', sep = ''))
    ptab <- matrix(cbind(x$condORs, x$cik), nrow = x$nstrata, 
                   dimnames = list(Strata = c(1:x$nstrata)))
    colnames(ptab) <- c('OR Est.', 'Lower', 'Upper')
    print(round(ptab,4))
  }
}



#### Marginal Odds Ratios ####
mor		<- function(tab){
  marginal	<- apply(tab, c(1,2), sum)
  thetaXY		<- marginal[1,1]*marginal[2,2]/(marginal[1,2]*marginal[2,1])
  return(thetaXY)
}
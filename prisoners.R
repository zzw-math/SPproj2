##empty dataframe
## Overview: In this project, we have 2n prisons who are numbered from 1-2n.
## We also have a room contained box numbered 1-2n, inside which is a card 
## numbered 1-2n.The prisoners go into the room to choose a box, if the number 
##on the card inside the box,the prisoner can be free. In the project, we 
##calculate the frequency and probability of release a prisoner.

##In the beginning, we define a function 'Escape', which contains 3 strategies.
##For Strategy 1, we define the result as FALSE, the prisoner with number k 
##choose the number k box, if the card inside the box has number k, the prisoner
##can be free. Then the result becomes TRUE.

Escape <- function(n, k, s, ranbox){
  result = FALSE
  if(s==1){
    box <- k 
    for (j in 1:n){
      card <- ranbox[box]
      if(k==card){
        result <- TRUE
        break
      }
    }
  }
## For Strategy 2, the prisoner with number k randomly choose a box, if the card 
## inside the box has number k, the prisoner can be free.
  else if(s==2){
    box <- sample(1:(2*n),1,replace=FALSE)
    for (j in 1:n){
      card <- ranbox[box]
      if(k==card){
        result <- TRUE
        break
      }
    }
  }
  ## For Strategy 3, the prisoner with number k choose n box at one time, 
  ##if the card inside the box has number k, the prisoner can be free.
  else if (s==3){
    box <- sample(1:(2*n), n, replace=FALSE)
    card <- ranbox[box]
    if(k%in%card){
      result <- TRUE
    }
  }
  return(result)
}
##Then we define a function 'Pone'. In Pone, we obtain the sum number of release 
##a prisoner and then find the probability. We generate a matrix showing the 
##state of release: if free, count as TRUE, if not free, count as FALSE.
##Then we sum up the number of TRUE and divided by the total number to get 
##probability.
Pone <- function(n, k, s, nerps){
  nreps <- 10000
  result <- rep(FALSE, nreps)
  for (i in c(1:nreps)){
    ranbox <- sample(1:(2*n), 2*n, replace=FALSE)
    result[i] <- Escape(n,k,s,ranbox)
  }
  prob <- sum(result)/nreps
  return(prob)
}

Pall <- function(n, s, nreps){
  nreps <- 10000
  M <- array(NA, c(nreps, 2*n))
  for (i in 1:nreps){
    ranbox <- sample(1:(2*n), 2*n, replace=FALSE)
    for (k in 1:(2*n)){
      M[i,k] <- Escape(n, k, s, ranbox, ranbox)
    }
  }
  release <- apply(M,1,sum)
  result <- release==2*n
  prob <- sum(result)/nreps
  return(prob)
}      

n <- 5
for (s in 1:3){
  inidprob <- rep(NA, 2*n)
  for (k in 1:(2*n)){
    inidprob[k] <- Pone(n, k, s)
  }
  jointprob <- Pone(n, k, s)
  cat(inidprob, '\n')
  cat(jointprob, '\n')
}

n <- 50
for (s in 1:3){
  inidprob2 <- rep(NA, 2*n)
  for (k in 1:(2*n)){
    inidprob2[k] <- Pone(n, k, s)
  }
  jointprob2 <- Pone(n, k, s)
  cat(inidprob2, '\n')
  cat(jointprob2, '\n')
}

##Q4: For strategy 1&2, they have the similar individual probability.
##In Strategy 1, we have (2*n)! permutation. If there exists a permutation within 
##length n, the prisoner will be released.
##In Strategy 2, the prison may choose the same box more than once, so the joint 
##probability is the lowest.
##In Strategy 3, we obtained the highest individual and 
##probability, however,
##For Strategy 3, the individual probability is no longer randomly independent,
##individual probability is around o.5. In this case the the joint probability 
##will be larger. And with the increase of n, the joint probability approaches 0.5.
##Overall, Strategy 3 is the optimal one.

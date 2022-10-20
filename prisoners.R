##Tianqi Dai Project2 Q1-Q4

## Overview: In this project, we have 2n prisons who are numbered from 1-2n.
## We also have a room contained box numbered 1-2n, inside which is a card 
## numbered 1-2n.The prisoners go into the room to choose a box, if the number 
## on the card inside the box is the same as prisoner's number, the prisoner can 
## be free. In the project,calculate the frequency and probability of release.

## In the beginning, we define a function 'Escape', which contains 3 strategies.
## For Strategy 1, we define the result as FALSE, the prisoner with number k 
##choose the number k box, if the card inside the box has number k, the prisoner
##can be free. Then the result becomes TRUE.If not, choose the new box with new
## card number
##  ranbox: random box from 1-2n
##  card: the random card inside the box
##  result: whether the prisoner can escape
##  s: strategy(1,2,3)
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
      box <- card
    }
  }
## For Strategy 2, the prisoner with number k randomly choose a box, if the card 
## inside the box has number k, the prisoner can be free.Otherwise, randomly 
## choose another box, and repeat the process.
  else if(s==2){
    box <- sample(1:(2*n),1,replace=FALSE)
    for (j in 1:n){
      card <- ranbox[box]
      if(k==card){
        result <- TRUE
        break
      }
      box <- card
    }
  }
## For Strategy 3, the prisoner with number k choose n box at one time, if the 
## card inside the box has number k, the prisoner can be free. Otherwise, the
## prisoner cannot be released.
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
##a prisoner and then find the probability. We generate a showing the state of 
##release: if free, count as TRUE, if not free, count as FALSE.Then we sum up 
##the number of TRUE, divided by the total number to get individual probability.
##  nreps: default number 10000
##  prob: probability to escape
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
## Define a function 'Pall', we obtain a matrix containing the result of joint
## escape situations, and then we produce the joint probability of every 
## strategy.
##  M: matrix that contains the TRUE/FALSE output
##  release: the situation of prisoners to be free
Pall <- function(n, s, nreps){
  nreps <- 10000
  M <- array(NA, c(nreps, 2*n))
  for (i in 1:nreps){
    ranbox <- sample(1:(2*n), 2*n, replace=FALSE)
    for (k in 1:(2*n)){
      M[i,k] <- Escape(n, k, s, ranbox)
    }
  }
  release <- apply(M,1,sum)
  result <- release==2*n
  prob <- sum(result)/nreps
  return(prob)
}      

## Input n = 5 & 50 to measure the probability of individual and joint 
## probability for prisoners to escape.
##  inidprob: individual probability to escape
##  jointprob: joint probability to escape
n <- 5
for (s in 1:3){
  inidprob <- rep(NA, 2*n)
  for (k in 1:(2*n)){
    inidprob[k] <- Pone(n, k, s)
  }
  jointprob <- Pall(n, s)
  cat(inidprob, '\n')
  cat(jointprob, '\n')
}

n <- 50
for (s in 1:3){
  inidprob2 <- rep(NA, 2*n)
  for (k in 1:(2*n)){
    inidprob2[k] <- Pone(n, k, s)
  }
  jointprob2 <- Pall(n, s)
  cat(inidprob2, '\n')
  cat(jointprob2, '\n')
}

##Q4: 
##In Strategy 1, we have (2*n)! permutation. If there exists a permutation 
##within length n, the prisoner will be released. In Strategy 2, the prison may 
##choose the same box more than once. Then the cycle permutation occurs, 
##resulting to a lower joint probability. For Strategy 3, the individual 
##probability approaches to 0.5.In this case the the joint probability equals 
##approaximately to (0.5)^n, which will be the lowest when n gets larger.
##Overall, Strategy 1 is the optimal one.



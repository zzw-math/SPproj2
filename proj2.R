## names          |  student id
## Ziwen Zhong    |  s2326022
## Wenzheng Zhang |  s2310185
## Tianqi Dai     |  s2302524

## github repo: https://github.com/zzw-math/SPproj2

## contribution
## Ziwen Zhong | Wenzheng Zhang | Tianqi Dai
##     40%     |       30%      |    30%

################################################################
## Practice 2
## Overview
## We have 2n prisoners in a room, 2n cards placed in 2n boxes separately and 
## randomly.Prisoners, cards, boxes are labeled with integer 1~2n. One can 
## escape if he has found the card with the same integer as himself when he 
## finished to open n boxes.

## We list 3 strategies and wish to find out which strategy has the highest 
## probability for a certain prisoner to escape and for all prisoners to escape.
## There are 3 functions to solve this problem:
##    Escape(n, k, strategy, box2card): For given boxes with cards and prisoner' 
##           number, simulate the result if the prisoner escape successfully.
##    Pone(n, k, strategy, nreps=10000): Run the 'Escape' function for nreps 
##          times, store the result and calculate the escape probability for the 
##          prisoner k. 
##    Pall(n, strategy, nreps=10000): Simulate nreps times, for each simulation, 
##          we fix the situation of boxes and cards, and Run 'Escape' function 
##          for every prisoner.
## Then we judge in each simulation if all prisoners escape successfully and 
## calculate the probability.

## In the end, we define the loop length, for a fixed k, we start from box k, 
## get card k1 in it, then open box k1, ..., the loop length is the quantity of 
## boxes we opened to get card k.
## We want to calculate the probability of each loop length occurs in one 
## experiment.
## The function 'dloop' are there to solve this:
##     dloop(n, nreps=10000): Simulate nreps times, in each simulation, we store 
##          the circumstance if loop length = k occurred.
## Then we count the times loop length = k occurred in nreps simulations, and 
## calculate the probability.
## We also plot a scatter plot of the probability of each loop length, when n=50.
################################################################

Escape <- function(n, k, strategy, box2card){
  ## summary
  ##     For given boxes with cards and prisoner' number, simulate the result
  ##     if the prisoner escape successfully.
  ## input
  ##     n: integer. half of the number of prisoners
  ##     k: integer. prisoner's number
  ##     strategy: integer(1, 2, or 3). we have 3 strategies how prisoner 
  ##               decides which box to open
  ##     box2card: vector of integer. a vector to store card's number in certain 
  ##               boxes eg. c(2, 3, 1, 4) means card 2 in box 1, card 3 in box2, 
  ##              card 1 in box 3, and card 4 in box 4
  ## output
  ##     result: bool. whether the prisoner escape successfully
  result = F              ## whether the prisoner k escape successfully
  if (strategy!=3){
    ## in strategy 1, we start from box k;
    ## in strategy 2, we start from a random box. 
    if (strategy==1) box <- k else box <- sample(1:(2*n), 1)
    for (j in 1:n) {
      ## in the for loop, we show the process that a prisoner opens a box whose 
      ## number is 'box', gets the card and store card's number in 'card'.
      ## if the card's number is k, we break the loop and set the result to true, 
      ## which means the prisoner escape successfully.
      ## if not, let 'box' be 'card', and we will open box 'box' in the next loop.
      card <- box2card[box]
      if (k==card){
        result <- T
        break
      }
      box <- card
    }
  } else{
    ## in strategy 3, we just open n different boxes and get n cards.
    ## judge if k in these cards, if so, set result be true, which means the 
    ## prisoner escape successfully.
    boxes <- sample(1:(2*n), n)
    cards <- box2card[boxes]
    if (k%in%cards){
      result <- T
    }
  }
  return(result)
}


Pone <- function (n, k, strategy, nreps=10000){
  ## summary
  ##     Run the 'Escape' function for nreps times, store the result and 
  ##     calculate the escape probability for the prisoner k. 
  ## input
  ##     n: integer. half of the number of prisoners
  ##     k: integer. prisoner's number
  ##     strategy: integer(1, 2, or 3). we have 3 strategies how prisoner decides 
  ##               which box to open
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: float. 
  ##        For given 'n', the probability of prisoner k to escape successfully. 
  result <- rep(F, nreps)
  ## whether the prisoner k escape successfully in 'nreps' experiments
  for (i in 1:nreps){
    ## simulate nreps times, in each simulation, we fix the situation of boxes 
    ## and cards, and simulate the result if prisoner k escape successfully.
    ## store the result in the vector 'result'
    box2card <- sample(1:(2*n), 2*n)
    result[i] <- Escape(n, k, strategy, box2card)
  }
  prob <- sum(result)/nreps
  return(prob)
}


## step 2
Pall <- function (n, strategy, nreps=10000){
  ## summary
  ##    Simulate nreps times, in each simulation, we fix the situation of boxes 
  ##    and cards, and Run 'Escape' function for every prisoner. Then we judge 
  ##    in each simulation if all prisoners escape successfully and calculate 
  ##    the probability.
  ## input
  ##     n: integer. half of the number of prisoners
  ##     strategy: integer(1, 2, or 3). we have 3 strategies how prisoner 
  ##              decides which box to open
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: float. For given 'n', the probability of prisoner k to escape 
  ##          successfully. 
  M <- array(NA,c(nreps, 2*n))    
  ## every row stores the result if each prisoner escape successfully, and we 
  ## have nreps rows to store each experiment
  for (i in 1:nreps){
    ## simulate nreps times, in each simulation, we fix the situation of boxes 
    ## and cards, and simulate the result if each prisoner escape successfully.
    ## store the result in the matrix M.
    box2card <- sample(1:(2*n), 2*n)
    for (k in 1:(2*n)) M[i,k] <- Escape(n, k, strategy, box2card)
  }
  ## count the number of the prisoners who escape successfully in each experiment.
  ## judge whether the count is 2n and calculate the probability.
  num_of_release <- apply(M, 1, sum)
  result <- num_of_release==2*n
  prob <- sum(result)/nreps
  return(prob)
}
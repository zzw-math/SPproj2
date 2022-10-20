## names          |  student id
## Ziwen Zhong    |  s2326022
## Wenzheng Zhang |  s2310185
## Tianqi Dai     |  s2302524

## github repo: https://github.com/zzw-math/SPproj2

## contribution
## Ziwen Zhong | Wenzheng Zhang | Tianqi Dai
##     40%     |       30%      |    30%


###############################################################################
## Practice 2
## Overview
## We have 2n prisoners in a room, 2n cards shuffled in 2n boxes separately.
## Prisoners, cards, boxes are labeled with integer 1~2n.
## One can escape if he has found the card with the same integer as himself when
## he finishes to open n boxes.

## We list 3 strategies and wish to find out which strategy has the highest 
## probability for a certain prisoner to escape and for all prisoners to escape.
## strategy 1: Prisoner k start from box k, get card k1 from it, open box k1, 
##             and repeat the process until get card k.
## strategy 2: As strategy 1, but starting from a randomly selected box.
## strategy 3: Open n boxes at random, checking each card for their number.

## There are 3 functions to calculate the individual and joint probability:
##     Escape: For given a shuffling of cards to boxes and prisoner's number, 
##             simulate if the prisoner escape successfully.
##     Pone:   Run the 'Escape' function for nreps times, store the result and
##             calculate the escape probability for the prisoner k. 
##     Pall:   Simulate nreps times, for each simulation, we fix a random shuffling 
##             of cards to boxes, and Run 'Escape' function for every prisoner.
##             Then we judge in each simulation if all prisoners escape successfully 
##             and calculate the probability.

## In the end, we define the loop length, for a fixed k, we start from box k, 
## get card k1 in it, then open box k1, ..., the loop length is the quantity of 
## boxes we opened before get card k. We want to calculate the probability of 
## each loop length occurs.
## We write fuction 'loop_occure' to record if each loop taking place 
## We also plot a bar chart of the probability of each loop length occurred,
## when n=50.

################################################################

Escape <- function(n, k, strategy, box2card){
  ## summary
  ##     For given boxes with cards and prisoner's number, 
  ##     simulate the result if the prisoner escape successfully.
  ## input
  ##     n: integer. half of the number of prisoners.
  ##     k: integer. prisoner's number.
  ##     strategy: integer(1, 2, or 3). we have 3 strategies how prisoner decides 
  ##               which box to open.
  ##     box2card: vector of integer. a vector to store a random shuffling of 
  ##               cards to boxes.
  ##               eg. c(2, 3, 1, 4) means card 2 in box 1, card 3 in box 2, 
  ##               card 1 in box 3, and card 4 in box 4
  ## output
  ##     result: bool. whether the prisoner escape successfully
  result = F              ## whether the prisoner k escape successfully
  if (strategy!=3){
    ## in strategy 1, we start from box k; in strategy 2, we start from a random 
    ## box. 
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
    ## in strategy 3, we just open n different boxes and get the cards.
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
  ##     n: integer. half of the number of prisoners.
  ##     k: integer. prisoner's number.
  ##     strategy: integer(1, 2, or 3). we have 3 strategies how prisoner 
  ##               decides which box to open.
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: float. For given 'n', the probability of prisoner k to escape 
  ##           successfully. 
  result <- rep(F, nreps)  ## whether the prisoner k escape successfully
  for (i in 1:nreps){
    ## Simulate nreps times, in each simulation, we fix a random shuffling of 
    ## cards to boxes, and simulate if prisoner k escape successfully.
    ## Store the result in the nreps length vector 'result'.
    box2card <- sample(1:(2*n), 2*n)
    result[i] <- Escape(n, k, strategy, box2card)
  }
  prob <- sum(result)/nreps
  return(prob)
}


Pall <- function (n, strategy, nreps=10000){
  ## summary
  ##    Simulate nreps times, in each simulation, we fix a random shuffling of
  ##    cards to boxes, and run 'Escape' function for every prisoner.
  ##    Then we judge in each simulation if all prisoners escape successfully 
  ##    and calculate the probability.
  ## input
  ##     n: integer. half of the number of prisoners.
  ##     strategy: integer(1, 2, or 3). we have 3 strategies how prisoner 
  ##               decides which box to open
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: float. For given 'n', the probability of prisoner k to escape
  ##           successfully. 
  M <- array(NA,c(nreps, 2*n))   ## every row stores the result if each prisoner 
                                 ## escape successfully, and we have nreps rows 
                                 ## to store each experiment
  for (i in 1:nreps){
    ## simulate nreps times, in each simulation, we fix a random shuffling of 
    ## cards to boxes, and simulate if each prisoner escape successfully.
    ## store the result in the matrix M.
    box2card <- sample(1:(2*n), 2*n)
    for (k in 1:(2*n)) M[i,k] <- Escape(n, k, strategy, box2card)
  }
  ## count the prisoners who escape successfully in each experiments.
  ## judge whether the count is 2n and calculate the probability.
  num_of_release <- apply(M, 1, sum)
  result <- num_of_release==2*n
  prob <- sum(result)/nreps
  return(prob)
}


## fix n=5 or 50, held the experiments for each strategy, calculate the individual 
## probability for certain prisoners to escape successfully,
## and the joint probability for all prisoners to escape successfully.
for (n in c(5, 50)){
  cat('when n =', n)
  for (strategy in 1:3){
    individual_prob <- rep(NA,2*n)    ## store the probability for a certain 
    ## prisoner to escape successfully.
    for (k in 1:(2*n)){
      individual_prob[k] <- Pone(n, k, strategy)
    }
    joint_prob <- Pall(n, strategy)   ## store the probability for all prisoners 
    ## to escape successfully.
    cat('In strategy', strategy, ',\nthe individual prob is:\n')
    cat(individual_prob,'\n')
    cat('the joint prob is:\n')
    cat(joint_prob,'\n')
  }
}


## Some Conclusions
## 1. The individual probabilities for each prisoner to escape successfully 
##    under strategy 2 is less than that of the others two.
## In strategy 1 and 3, prisoner k will never back to an opened box until they 
## get card k. Thus they don't waste their chance to open n different boxes. So 
## the probability is 0.5.
## In strategy 2, prisoner start from box k0 might get card k0 in the procedure, 
## which will lead to a loop and waste their chance. So the probability is less 
## than 0.5.

## 2. The joint probability for all prisoners to escape successfully under 
##    strategy 1 is much bigger than that of the other two.
## It's obviously that in strategy 2 and 3, the events of each prisoner free are
## independent. So the joint probability is the product of the individual 
## probabilities, which lead to a very small probability.
## However, in strategy 1, the event of all prisoner free is up to all the loop 
## length no bigger than n, which has a significant probability.


loop_occure <- function(n) {
  ## summary 
  ##    For a given n, record if each loop length from 1 to 2n occurred.
  ## input
  ## output
  result <- rep(F, 2*n)
  box2card <- sample(1:(2*n), 2*n)
  ## fix a random shuffling of cards to boxes first, then set loop length=1 
  ## and start from box k, if we get card k in box k, just return the loop 
  ## length and break the 'for loop', if not, plus loop length with 1, set
  ## 'box'='card', and open box 'box' in the next loop, until we get card k.
  ## It's obviously that we will definitely get card k after we open all the 
  ## boxes, which is, opening boxes for 2n times. So, we simply use 'for loop',
  ## and, we can also use 'while loop' begin with 'while True'.
  for (k in 1:(2*n)){
    loop_length <- 1
    box <- k
    for (j in 1:(2*n)){
      card <- box2card[box]
      if (card==k){
        result[loop_length] <- T
        break
      }
      box <- card
      loop_length <- loop_length + 1
    }
  }
  return(result)
}

dloop <- function (n, nreps=10000){
  ## summary
  ##     simulate nreps times, in each simulation, for loop length from 1 to 2n,
  ##     we record if the loop length occurred. Then we count the times loop 
  ##     length = k occurred in nreps experiments, and calculate the probability.
  ## input
  ##     n: integer. half of the number of boxes(cards)
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: vector(float). For given 'n', the probability for loop length 
  ##           from 1 to 2n occurred. 
  result <- array(F, c(nreps, 2*n))     ## every row stores the result if each 
                                        ## loop length occurred, and we have 
                                        ## nreps rows to store each experiment.
  for (i in 1:nreps){
    result[i,] <- loop_occure(n) 
  }
  prob <- apply(result, 2, sum)/nreps
  return(prob)
}


loop_no_longer_than_n <- function(n, nreps=10000){
  count <- 0    ## to store the
  for (i in 1:nreps){
    result <- loop_occure(n)
    if(sum(result[51:(2*n)])==0) count <- count + 1
  }
  prob <- count/nreps
  return(prob)
}


## fix n=50, calculate the probability for each loop length occurred, and 
## visualize it by bar chart.
n <- 50
prob <- dloop(n)
barplot(prob, names.arg=1:(2*n), xlab='loop length', ylab='probability of occuring')
no_loop_longer_than_50 <- loop_no_longer_than_n(50)
cat('when n = 50, the probability that there is no loop longer than 50 is ')
cat(no_loop_longer_than_50)







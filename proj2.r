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
## We have 2n prisoners in a room, 2n cards shuffled into 2n boxes separately.
## Prisoners, cards, boxes are labeled with integer 1~2n.
## One can escape if one has found the card with the same number with one itself
## after opening n boxes.

## We list out 3 strategies and wish to find out the one has the highest 
## probability for the certain and all prisoners to escape.
## strategy 1: Prisoner k start from box k, get card k1 from it, open box k1, 
##             and repeat the process until get card k.
## strategy 2: As strategy 1, but starting with a randomly selected box.
## strategy 3: Opening n boxes at random, checking each card for their number.

## There are 3 functions to calculate the individual and joint probability:
##     Escape: For given n, strategy, prisoner's number, and a shuffling cards  
##             to boxes, simulate the result if the prisoner successfully escape.
##     Pone:   For given n, prisoner's number, and strategy. Run the 'Escape' 
##             function for nreps times, store the result and calculate the 
##             escape probability for the prisoner k.  
##     Pall:   For given n and strategy, run the 'Escape' for nreps times, store
##             the result and calculate the probability for all prisoners to 
##             escape.

## In the end, we define the loop length, for a fixed k, we start from box k, 
## get card k1 in it, then open box k1, ..., the loop length is the quantity of 
## boxes we opened before get card k. We want to calculate the probability of 
## each loop length occurs.

## loop_occure: For a given n and a shuffling of cards to boxes. check if each  
##              loop length from 1 to 2n occurred.
## dloop:       For a given n, calculate the probability of each loop occurred.
## loop_no_longer_than_n: For a given n, calculate the probability that there is 
##              not loop longer than n.

## We plot a bar chart of the probability of each loop length occurred, when
## n=50.

################################################################

Escape <- function(n, k, strategy, box2card){
  ## summary
  ##     For given n, strategy, prisoner's number, and a shuffling cards to 
  ##     boxes, simulate the result if the prisoner successfully escape.
  ## input
  ##     n: integer. half of the number of prisoners.
  ##     k: integer. prisoner's number.
  ##     strategy: integer(1, 2, or 3). Strategies(1, 2, 3) are listed above.
  ##     box2card: vector of integer. A vector stores a random shuffling of 
  ##               cards to boxes.
  ##               e.g. c(2, 3, 1, 4) means card 2 in box 1, card 3 in box 2, 
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
    ## determine whether k is in these cards, if it is, set result be true,   
    ## which means the prisoner escape successfully.
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
  ##     For given n, prisoner's number, and strategy. Run the 'Escape' function
  ##     for nreps times, store the result and calculate the escape probability 
  ##     for the prisoner k.  
  ## input
  ##     n: integer. half of the number of prisoners.
  ##     k: integer. prisoner's number.
  ##     strategy: integer(1, 2, or 3). 
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: float. For given 'n', the probability of prisoner k to escape 
  ##           successfully. 
  result <- rep(F, nreps)  ## whether the prisoner k escape successfully
  for (i in 1:nreps){
    ## Simulate nreps times, in each simulation, we fix a random shuffling of 
    ## cards to boxes, and simulate if prisoner k escape successfully.
    ## Store the result in the nreps length of vector 'result'.
    box2card <- sample(1:(2*n), 2*n)
    result[i] <- Escape(n, k, strategy, box2card)
  }
  prob <- sum(result)/nreps
  return(prob)
}


Pall <- function (n, strategy, nreps=10000){
  ## summary
  ##    For given n and strategy, run the 'Escape' for nreps times, store the
  ##    result and calculate the probability for all prisoners to escape.
  ## input
  ##     n: integer. half of the number of prisoners.
  ##     strategy: integer(1, 2, or 3).
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: float. For given 'n', the probability of all prisoners to escape
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
  ## determine whether the count is 2n and calculate the joint probability.
  num_of_release <- apply(M, 1, sum)
  result <- num_of_release==2*n
  prob <- sum(result)/nreps
  return(prob)
}


## Fix n=5 or 50, held the experiments for each strategy, calculate the individual 
## probability for the certain and all prisoners to successfully escape.
for (n in c(5, 50)){
  cat('when n =', n, '\n')
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
##    under strategy 2 is much lesser in comparison with other two.
## In strategy 1 and 3, prisoner k will never back to an opened box until they 
## get card k. Thus they don't waste their chance to open n different boxes. So 
## the probability is around 0.5.
## In strategy 2, prisoner start from box k0 might get card k0 in the procedure, 
## which will lead to a repeating loop and waste their chance. So, the 
## probability is less than 0.5.
## e.g. if there are 4 boxes k0, k1, k2, k3 contain 4 cards k1, k2, k3, k0 
## respectively, when we start from box k0, the route will be
## k0 -> k1 -> k2 -> k3 -> k0.

## 2. The joint probability for all prisoners to escape successfully under 
##    strategy 1 is much bigger than that of the other two.
## It's obviously that in strategy 2 and 3, the events of each prisoner free are
## independent. So the joint probability is the product of the individual 
## probabilities, which lead to a very small probability.
## However, in strategy 1, the event of all prisoners free depend on all the loop 
## length no bigger than n, which has a significant probability.


loop_occure <- function(n, box2card) {
  ## summary 
  ##    For a given n and a shuffling of cards to boxes. check if each loop 
  ##    length from 1 to 2n occurred.
  ## input
  ##       n: integer. half of the number of prisoners.
  ## output
  ##     result: vector(bool). in one experiment, if each loop length from 1 to  
  ##             2n occurred, record it with True.
  result <- rep(F, 2*n)
  ## set loop length=1, and start from box k, if we get card k in box k, just 
  ## return the loop length and break the 'for loop', if not, plus loop length
  ## with 1, set 'box'='card', and open box 'box' in the next loop, until we
  ## get card k. It's obviously that we will definitely get card k after we open
  ## all the boxes, which is, opening boxes for 2n times. So, we simply use
  ## 'for loop', and, we can also use 'while loop' begin with 'while True'.
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
  ##     For a given n, calculate the probability of each loop occurred.
  ## input
  ##     n: integer. half of the number of boxes(cards)
  ##     nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##     prob: vector(float). For given 'n', the probability for each loop  
  ##           length from 1 to 2n occurred. 
  result <- array(F, c(nreps, 2*n))     ## every row stands for one experiment,
                                        ## stores the result if each loop length
                                        ## occurred.
  for (i in 1:nreps){
    ## simulate nreps times, in each experiment, fix a random shuffling of cards 
    ## to boxes, record if the loop length occurred.
    box2card <- sample(1:(2*n), 2*n)
    result[i,] <- loop_occure(n, box2card) 
  }
  ## count the frequence of each loop length in nreps experiments, and calculate 
  ## the probability.
  prob <- apply(result, 2, sum)/nreps
  return(prob)
}


loop_no_longer_than_n <- function(n, nreps=10000){
  ## summary 
  ##    For a given n, calculate the probability that there is not loop longer 
  ##    than n.
  ## input
  ##    n: integer. half of the number of boxes(cards)
  ##    nreps: integer(default to 10000). the number of simulation times.
  ## output
  ##    prob: float. For given 'n', the probability that there is not loop 
  ##          longer than n
  count <- 0    ## to store the quantity of loops that are no longer than n
  for (i in 1:nreps){
    ## simulate nreps times of experiments, in each experiment, fix a shuffling
    ## of cards to boxes. if there does not exist the loops longer than 50, add 
    ## one to 'count'.
    box2card <- sample(1:(2*n), 2*n)
    result <- loop_occure(n, box2card)
    if(sum(result[51:(2*n)])==0) count <- count + 1
  }
  ## divide count with nreps to calculate the probability.
  prob <- count/nreps
  return(prob)
}


## fix n=50, calculate the probability for each loop length occurred, and 
## visualize it by bar chart.
n <- 50
prob <- dloop(n)
barplot(prob, names.arg=1:(2*n), xlab='loop length', ylab='probability of occuring')
no_loop_longer_than_50 <- loop_no_longer_than_n(50)
cat('when n = 50, the probability that there is no loop longer than 50 is', 
    no_loop_longer_than_50)


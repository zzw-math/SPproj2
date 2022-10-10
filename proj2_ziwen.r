## names        |  student id
## Ziwen Zhong  |  s2326022

## github repo: https://github.com/zzw-math/SPproj2

## contribution


################################################################


## step 1
Pone <- function (n, k, strategy, nreps=10000){
  result <- rep(F, nreps)
  for (i in 1:nreps){
    box2card <- sample(1:(2*n),2*n)
    if (strategy!=3){
      if (strategy==1) box <- k else box <- sample(1:(2*n),1)
      for (j in 1:n) {
        card <- box2card[box]
        if (k==card){
          result[i] <- T
          break
        }
        box <- card
      }
    } else{
      boxes <- sample(1:(2*n),n)
      cards <- box2card[boxes]
      if (k%in%cards){
        result[i] <- T
      }
    }
  }
  prob <- sum(result)/nreps
  return(list(result=result,prob=prob))
}


## step 2
Pall <- function (n, strategy, nreps=10000){
  M <- array(NA,c(nreps,2*n))
  for (k in 1:(2*n)) M[,k] <- Pone(n,k,strategy,nreps)$result
  num_of_release <- apply(M, 1, sum)
  result <- num_of_release==2*n
  prob <- sum(result)/nreps
  return(prob)
}


## step 3
n <- 5
for (strategy in 1:3){
  individual_prob <- rep(NA,2*n)
  for (k in 1:(2*n)){
    individual_prob[k] <- Pone(n, k, strategy)$prob
  }
  joint_prob <- Pall(n, strategy)
  cat('when using strategy',strategy, ',\nthe individual prob is:\n')
  cat(individual_prob,'\n')
  cat('the joint prob is:\n')
  cat(joint_prob,'\n')
}


## step 4
## The individual success probabilities under strategy 2
## is less than that of the others


## step 5
dloop <- function (n, nreps=10000){
  result <- array(F,c(nreps,2*n))
  for (i in 1:nreps){
    box2card <- sample(1:(2*n), 2*n)
    for (k in 1:(2*n)){
      loop_length <- 1
      box <- k
      for (j in 1:(2*n)){
        card <- box2card[box]
        if (card==k){
          result[i,loop_length] <- T
          break
        }
        box <- card
        loop_length <- loop_length + 1
      }
    }
  }
  prob <- apply(result, 2, sum)/nreps
  return(prob)
}


## step 6
n <- 50
x <- 1:(2*n)
prob <- dloop(n)
plot(x, prob,xlab='loop length', ylab='pobability of occuring')
abline(h=0,col='red')










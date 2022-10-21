Pone <- function(n, k, strategy, nreps=10000) {
  boxes_lable <- 1:(2*n)
  leave_count <- 0
  if (strategy == 1) {
    for (j in 1:nreps) {
      cards_lable <- sample(2*n, 2*n)
      boxes_lable_first <- k
      cards_number <- cards_lable[boxes_lable_first]
      for (i in 1:n) {
        if (k == cards_number) {
          leave_count  <- leave_count + 1
          break
        } else {cards_number <- cards_lable[boxes_lable[cards_number]]}
      }
    }
  }
  if (strategy == 2) {
    for (j in 1:nreps) {
      cards_lable <- sample(2*n, 2*n)
      boxes_lable_first <- sample(2*n, 1)
      cards_number <- cards_lable[boxes_lable_first]
      for (i in 1:n) {
        if (k == cards_number) {
          leave_count <- leave_count + 1
          break
        } else {cards_number <- cards_lable[boxes_lable[cards_number]]}
      }
    }
  }
  if (strategy == 3) {
    for (j in 1:nreps) {
      cards_lable <- sample(2*n, 2*n)
      random_pick_boxes <- sample(2*n, n)
      if (k %in% cards_lable[random_pick_boxes]) {
        leave_count <- leave_count + 1
      }
    }
  }
  return(leave_count/nreps)
}     
## cat(Pone(1000, 5, 1)); cat(Pone(1000, 5, 2)); cat(Pone(1000, 5, 3))
 
Pall <- function(n, strategy, nreps=10000) {
  all_leave <- 0
  for (j in 1:nreps) {
    leave <- 0
    for (i in 1:(2*n)) {
      if (Pone(n, i, strategy, nreps=1) == 1) {
        leave <- leave + 1
      }
    }
    if (leave == (2*n)) {
      all_leave <- all_leave + 1
    }
  }
  return(all_leave/nreps)
}








 


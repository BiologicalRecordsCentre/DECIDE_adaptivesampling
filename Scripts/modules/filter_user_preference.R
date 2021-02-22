filter_user_preference <- function(userID = 1){
  
  user_list <- c('Polyommatus bellargus')
  
  attr(user_list, which = 'method') <- 'user_specfied'
  
  return(user_list)
  
}

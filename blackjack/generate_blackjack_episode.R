##############################################################
### The R code for the Blackjack example (example 5.1)
##############################################################

############# Simulating the game ################
## actions: 0-stick; 1-hit. 

## deal cards
deal <- function(){
  x <- min(sample.int(13, 1), 10)
  return(x)
}

## check if blackjack (i.e. "A10", "AJ", "AQ", "AK")
check_blackjack <- function(x){
  return( ifelse((1%in%x) & (10%in%x) & (length(x)==2), TRUE, FALSE) )
}

## check if going bust (i.e. excess of 21)
check_bust <- function(x){
  return( ifelse(sum(x)>21, TRUE, FALSE) )
}

## count (when not going bust)
count <- function(x){
  if (check_bust(x)==TRUE){
    cat("Exceed 21!\n")
    return(-Inf)
  } else {
    n1 <- sum(x==1)
    total_0 <- sum(x)
    extra <- 10 * (0:n1)
    total_all <- total_0 + extra
    total_1 <- max(total_all[total_all<=21])
    return(total_1)
  }
}

## dealer's policy
policy_dealer <- function(x){
  total <- count(x)
  n1 <- sum(x==1)
  if (total >= 18){
    return (0)
  } else if ( (total==17) & (n1==0)){
    return (0)
  } else {
    return (1)
  }
}

## initial check (if any blackjack)
initial_check <- function(x1, x2){
  ## x1:dealer, x2:player
  c1 <- check_blackjack(x1)
  c2 <- check_blackjack(x2)
  if (c1==TRUE & c2==TRUE){
    return (0)
  } else if (c1==TRUE & c2==FALSE){
    return (-1)
  } else if (c1==FALSE & c2==TRUE){
    return (1)
  } else {
    return (2)
  }
}

## player's turn
player_round <- function(x, y, policy_player){
  MDP <- NULL
  while (1){
    s <- count(x)
    ace <- (1%in%x)
    a <- policy_player(s, y, ace)
    MDP <- rbind(MDP, c(s, y, ace, a))
    if (a==1){
      x <- c(x, deal())
      if (check_bust(x)){
        break
      }
    } else {
      break
    }
  }
  colnames(MDP) <- c("count", "dealer_card", "ace", "action")
  return(list(x=x, MDP=MDP))
}

## dealer's turn
dealer_round <- function(x){
  while (1){
    a <- policy_dealer(x)
    if (a==1){
      x <- c(x, deal())
      if (check_bust(x)) break
    } else {
      break
    }
  }
  return(x)
}

## final check (compare counts)
final_check <- function(x1, x2){
  s1 <- count(x1)
  s2 <- count(x2)
  if (s1 > s2){
    return (-1)
  } else if (s1 < s2){
    return (1)
  } else {
    return (0)
  }
}

## play game (one episode) 
play_blackjack <- function(policy_player){
  ## deal cards
  x1 <- c(deal(), deal()) # dealer
  x2 <- c(deal(), deal()) # player
  ## check if blackjack
  out1 <- initial_check(x1, x2)
  if (out1!=2){
    return (list(r=out1, dealer=x1, player=x2, class="blackjack", MDP=NULL))
  } else {
    ## player's turn
    out2 <- player_round(x=x2, y=x1[1], policy_player)
    x2 <- out2$x
    MDP <- out2$MDP
    if (check_bust(x2)){ ## check player goes bust or not
      return (list(r=-1, dealer=x1, player=x2, class="bust", MDP=MDP))
    } else {
      ## dealer's turn
      x1 <- dealer_round(x1)
      if (check_bust(x1)){ ## check dealer goes bust or not
        return (list(r=1, dealer=x1, player=x2, class="bust", MDP=MDP))
      } else {
        r <- final_check(x1, x2)
        return (list(r=r, dealer=x1, player=x2, class="normal", MDP=MDP))
      }
    }
  }
}

## player's policy (we can change it)
policy_player_my <- function(s, y, ace){
  #s: count of player's cards; 
  #y: dealer's first card; 
  #ace: [true/false] whether player have usable ace).
  if (s >= 17){
    return (0)
  } else {
    return (1)
  }
}

## player's policy (random actions with p=0.5)
policy_player_random <- function(s, y, ace){
  #s: count of player's cards; 
  #y: dealer's first card; 
  #ace: [true/false] whether player have usable ace).
  return(rbinom(1, 1, 0.5))
}

## player's policy (optimal based on the book)
policy_player_optimal <- function(s, y, ace){
  #s: count of player's cards; 
  #y: dealer's first card; 
  #ace: [true/false] whether player have usable ace).
  if (ace){
    if (y>=2 & y<=8){
      a <- ifelse(s>=18, 0, 1)
    } else {
      a <- ifelse(s>=19, 0, 1)
    }
  } else {
    if (y<=1 | y>=7){
      a <- ifelse(s>=16, 0, 1)
    } else if (y>=2 & y<=3){
      a <- ifelse(s>=13, 0, 1)
    } else {
      a <- ifelse(s>=12, 0, 1)
    }
  }
  return(a)
}






#################### run for one episode ####################
# play_blackjack(policy_player=policy_player)



# ############## compare three different policy ##############
# N_mc <- 2e5
# 
# ##
# R1 <- rep(0, N_mc)
# for (i in 1:N_mc){
#   episode <- play_blackjack(policy_player=policy_player_random)
#   R1[i] <- episode$r
# }
# table(R1) / N_mc
# mean(R1)
# 
# ##
# R2 <- rep(0, N_mc)
# for (i in 1:N_mc){
#   episode <- play_blackjack(policy_player=policy_player_my)
#   R2[i] <- episode$r
# }
# table(R2) / N_mc
# mean(R2)
# 
# ##
# R3 <- rep(0, N_mc)
# for (i in 1:N_mc){
#   episode <- play_blackjack(policy_player=policy_player_optimal)
#   R3[i] <- episode$r
# }
# table(R3) / N_mc
# mean(R3)



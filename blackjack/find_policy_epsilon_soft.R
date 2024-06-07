##############################################################
### The R code for the Blackjack example (example 5.1)
##############################################################
library(ggplot2)
source("generate_blackjack_episode.R")

## set the value for epsilon
epsilon <- function(){
  return (0.1)
}

############# Simulating the game (with epsilon-soft policy) ################

## player's initial policy (epsilon-soft policy)
policy_player_init <- function(s, y, ace, ref){
  a1 <- ifelse(s>=17, 0, 1)
  a2 <- rbinom(1, 1, 0.5)
  a <- ifelse(runif(1) > epsilon(), a1, a2)
  return(a)
}

## play game (one episode with fixed start) 
play_blackjack_fixed <- function(s, y, ace, policy_player, ref){
  #s: count of player's cards (12 to 20) 
  #y: dealer's first card (1 to 10) 
  #ace: whether player have usable ace (0/1).
  x1 <- c(y, deal()) # dealer
  if (ace) {
    x2 <- c(1, s-11) # player
  } else {
    x2 <- c(floor(s/2), s-floor(s/2))
  }
  ## check if blackjack
  out1 <- initial_check(x1, x2)
  if (out1!=2){
    return (list(r=out1, dealer=x1, player=x2, class="blackjack", MDP=NULL))
  } else {
    ## player's turn
    out2 <- player_round(x=x2, y=x1[1], policy_player, ref)
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


###### Policy evaluation of Q(s,a) via Monte Carlo with epsilon-soft policy ########

## formulate of the output
create_output <- function(){
  ref <- as.data.frame(expand.grid(12:20, 1:10, 0:1))
  names(ref) <- c("count", "dealer_card", "ace")
  ref$Q_a0 <- 0
  ref$Q_a1 <- 0
  return(ref)
}

## evaluation function
policy_eva <- function(N, policy_player, ref, learning_rate){
  
  ## initialize output
  if (!is.null(ref)){
    ref1 <- ref
  } else {
    ref1 <- create_output()
  }
  nn <- nrow(ref1)
  
  ## evaluate Q(s,a) by Monte Carlo methods
  DD <- matrix(0, 5*N, 5)
  stop_idx <- 0
  for (i in 1:N){
    index_i <-  sample(nn, 1)
    s <- ref1$count[index_i]
    y <- ref1$dealer_card[index_i]
    ace <- ref1$ace[index_i]
    episode <- play_blackjack_fixed(s, y, ace, policy_player, ref)
    if (episode$class!="blackjack"){
      D_i <- episode$MDP
      n_i <- nrow(D_i)
      DD[stop_idx+(1:n_i), 1:4] <- D_i
      DD[stop_idx+(1:n_i), 5] <- episode$r
      stop_idx <- stop_idx + n_i
      if (stop_idx >= 5*N-21){
        break
      }
    }
  }
  DD <- as.data.frame(DD[1:stop_idx,])
  names(DD) <- c("count", "dealer_card", "ace", "action", "Q")
  DD <- DD[DD$count>=12 & DD$count<=20,]
  Q_all <- aggregate(Q~count+dealer_card+ace+action, data = DD, mean)
  
  ## fill output with Q values
  for (k in 1:nrow(Q_all)){
    row_idx <- which(Q_all[k,1]==ref1[,1] & Q_all[k,2]==ref1[,2] & Q_all[k,3]==ref1[,3])
    col_idx <- Q_all$action[k] + 4
    ref1[row_idx, col_idx] <- Q_all$Q[k]
  }
  
  ## Qt(s,a) <- (1-learning_rate) * Qt-1(s,a) + learning_rate * Qt(s,a).
  if (!is.null(ref)){
    ref1$Q_a0 <- learning_rate * ref1$Q_a0 + (1-learning_rate) * ref$Q_a0
    ref1$Q_a1 <- learning_rate * ref1$Q_a1 + (1-learning_rate) * ref$Q_a1
  }
  
  ## update policy by finding: a_new = argmax{ Q(s,a) }
  ref1$a_new <- as.numeric(ref1$Q_a1 > ref1$Q_a0)
  return(ref1)
}

## player's policy improvement
policy_player_update <- function(s, y, ace, ref){
  if (s==21){
    return (0)
  } else if (s <=11) {
    return (1)
  } else {
    idx <- which(s==ref$count & y==ref$dealer_card & ref$ace==ace)
    a1 <- ref$a_new[idx]
    a2 <- rbinom(1, 1, 0.5)
    a <- ifelse(runif(1) > epsilon(), a1, a2)
    return(a)
  }
}



###### Policy iteration via Monte Carlo with epsilon-soft policy ########
policy_iter <- function(N=5e4, policy_player_init, visualize=FALSE, 
                        M_iter=100, learning_rate=0.1){
  
  ref0 <- policy_eva(N, policy_player_init, NULL, NULL)
  
  for (ii in 1:M_iter){
    cat("iter = ", ii, "\n")
    ##### visualize value functions
    if (visualize){
      ## plot Q difference
      pp <- ggplot(data = ref0) + 
        geom_tile(aes(x = count, y = dealer_card, 
                      fill = sign(Q_a0-Q_a1) * (abs(Q_a0-Q_a1))^(0.25) ), 
                  colour = "black") +
        facet_wrap(~ace, ncol=2, labeller = label_both) + 
        xlab("Player sum") + 
        ylab("Dealer showing") + 
        scale_fill_gradient2(high="red", low="blue", mid="white", 
                             midpoint = 0, name="Q(s,0)-Q(s,1)") +
        theme_classic(base_size = 20)
      print(pp)
    }
    ref1 <- policy_eva(N, policy_player_update, ref0, learning_rate)
    if ( sum(ref0$a_new!=ref1$a_new) == 0 ){
      break
    } else {
      ref0 <- ref1
    }
  }
  return(list(iter=ii, PD=ref1))
}






##########################################################################
################ Run Exploring Start Monte Carlo #########################
##########################################################################
N <- 5e5
o <- policy_iter(N, policy_player_init, visualize=TRUE, 
                 M_iter=100, learning_rate=0.1)


####################### visualize optimal policy #################
ggplot(data = o$PD) + 
  geom_tile(aes(x = count, y = dealer_card, fill = as.factor(a_new)), 
            colour = "black", alpha=0.7) +
  facet_wrap(~ace, ncol=2, labeller = label_both) + 
  scale_fill_discrete(name="Policy") + 
  xlab("Player sum") + 
  ylab("Dealer showing") +
  theme_classic(base_size = 20)


##################### check performance #################
## player's deterministic policy with ref
policy_player_deterministic <- function(s, y, ace, ref){
  if (s==21){
    return (0)
  } else if (s <=11) {
    return (1)
  } else {
    idx <- which(s==ref$count & y==ref$dealer_card & ref$ace==ace)
    return(ref$a_new[idx])
  }
}

R <- rep(0, N)
for (i in 1:N){
  episode <- play_blackjack(policy_player=policy_player_update, o$PD)
  R[i] <- episode$r
}
table(R) / N
mean(R)

R1 <- rep(0, N)
for (i in 1:N){
  episode <- play_blackjack(policy_player=policy_player_deterministic, o$PD)
  R1[i] <- episode$r
}
table(R1) / N
mean(R1)


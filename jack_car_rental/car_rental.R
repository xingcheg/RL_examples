##############################################################
### The R code for the Jack’s Car Rental example (example 4.2)
##############################################################
library(ggplot2)

##### states: number of cars at (location 1, location 2) at the end of the day.
##### s = (s1, s2), (s1, s2 = 0,1,...,20).

##### actions: the numbers of cars moved from location 1 to location 2.
##### a = {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5} 
##### rewards: $10 for renting, -$2 for moving.



######### MDP function to compute p(s' | s, a) and E(r | s, a) ##########
MDP_prob <- function(s, a){
  ##need to ensure: max(s[1]-20, -s[2], -5) <= a <= min(s[1], 20-s[2], 5)
  
  ##### update state #####
  s[1] <- s[1] - a
  s[2] <- s[2] + a
  
  ##### compute E(r|s,a) #####
  R1 <- -2 * abs(a)        #cost of moving cars
  pd1 <- prob_rent(s[1], lambda = 3)
  pd2 <- prob_rent(s[2], lambda = 4)
  p1 <- pd1$prob
  p2 <- pd2$prob
  R2 <- 0                 # expectation of money earned
  for (j in 0:s[1]){
    for (k in 0:s[2]){
      r <- 10 * (j+k)* p1[j+1]*p2[k+1]
      R2 <- R2 + r
    }
  } 
  R_expected <- R1  + R2   
  
  ##### compute p(s'|s,a) #####
  p_end1 <- prob_end(s[1], lambda=3, p1)
  p_end2 <- prob_end(s[2], lambda=2, p2)
  P_trans <- c(tcrossprod(p_end1, p_end2))
  
  return(list(P_trans = P_trans, R_expected = R_expected))
}

########## probability of # of cars rented for one location ##########
prob_rent <- function(current, lambda){
  if (current==0){
    p <- 1
  } else {
    p <- dpois(x = 0:(current-1), lambda = lambda)
    p <- c(p, 1-sum(p))
  }
  return(data.frame(rent=0:current, prob=p))
}

########## probability of # of cars at night for one location ##########
prob_end <- function(current, lambda, p_rent){
  p_end <- rep(0, 21)
  p_rent <- 
    p_rent_rev <- p_rent[length(p_rent):1]
  for (j in 0:current){ ## j is the number of cars after rent
    if (j==20){
      p <- 1
    } else {
      p <- dpois(x = c(0:(19-j)), lambda = lambda)
      p <- c(p, 1-sum(p))
    }
    idx <- (j:20)
    p_end[idx+1] <- p_end[idx+1] + p * p_rent_rev[j+1]
  }
  return(p_end)
}

## policy function (initial)
policy0 <- function(s, a){
  p <- ifelse(a==0, 1, 0)
  return (p)
}

############# policy evaluation #############
policy_eva <- function(gamma, policy, ...){
  s_all <- expand.grid(0:20, 0:20)
  m <- nrow(s_all)
  R <- rep(0, m)
  H <- matrix(0, m, m)
  for (j in 1:m){
    for (a in (-5:5)){
      s <- as.numeric(s_all[j,])
      lb <- max(s[1]-20, -s[2])
      ub <- min(s[1], 20-s[2])
      if ((a>=lb) & (a<=ub)) { ## check if a is proper
        p_as <- policy(s, a, ...)
        mdp_out <- MDP_prob(s, a)
        r <- mdp_out$R_expected
        p_ssa <- mdp_out$P_trans
        R[j] <- R[j] + p_as * r
        H[j,] <- H[j,] + p_as * p_ssa
      }
    }
  }
  
  V <- solve(diag(m) - gamma * H) %*% R
  V_mat <- matrix(V, 21, 21)
  return(V_mat)
}


##
########## Policy iteration ##########
Q_value <- function(gamma, V){
  s_all <- expand.grid(0:20, 0:20)
  m <- nrow(s_all)
  a_all <- (-5:5)
  out <- matrix(-1e4, m, length(a_all))
  for (j in 1:m){
    s <- as.numeric(s_all[j,])
    for (a in a_all){
      lb <- max(s[1]-20, -s[2])
      ub <- min(s[1], 20-s[2])
      if ((a>=lb) & (a<=ub)) { ## check if a is proper
        mdp_out <- MDP_prob(s, a)
        r <- mdp_out$R_expected
        p_ssa <- mdp_out$P_trans
        out[j,a+6] <- r + gamma * crossprod(p_ssa, V)
      }
    }
  }
  return(out)
}

policy_iter <- function(gamma, policy0, tol=1e-3){
  V_mat0 <- policy_eva(gamma=gamma, policy=policy0)  # V value for initial policy
  V0 <- c(V_mat0)
  Q0 <- Q_value(gamma, V0)                           # Q value for initial policy
  dict0 <- apply(Q0, 1, which.max) - 6               # update policy
  ii <- 0 
  while(1){
    ii <- ii + 1
    V_mat1 <- policy_eva(gamma=gamma, policy=policy_improve, dict=dict0) # V value for updated policy
    V1 <- c(V_mat1)
    policy_mat0 <- matrix(dict0, 21, 21)
    pp <- plot_policy(dict0, title=paste0("Iteration = ", ii))
    print(pp)
    if (max(abs(V1 - V0)) < tol ){                   # convergence condition 
      break
    } else {
      V0 <- V1
      Q0 <- Q_value(gamma, V0)                       # Q value for updated policy
      dict0 <- apply(Q0, 1, which.max) - 6           # update policy
    }
  }
  return(list(iter=ii, 
              V_mat = V_mat1, 
              policy = policy_mat0))
}


policy_improve <- function(s, a, dict){
  s_all <- expand.grid(0:20, 0:20)
  k <- which( (s[1]==s_all[,1]) & (s[2]==s_all[,2]) )
  p <- ifelse(a==dict[k], 1, 0)
  return(p)
}


plot_policy <- function(dict, title, if_policy=TRUE){
  PD <- as.data.frame(expand.grid(0:20, 0:20))
  names(PD) <- c("s1", "s2")
  PD$Actions <- dict
  pp <- ggplot(data = PD) + 
    geom_raster(aes(x = s1, y = s2, fill = Actions)) +
    xlab("# of Cars at Location 1") + 
    ylab("# of Cars at Location 2") + 
    ggtitle(title) + 
    theme_bw(base_size = 14)
  if (if_policy){
    pp <- pp + 
      scale_fill_gradient2(low="blue", high="red", mid="white", 
                           midpoint = 0) 
  }
  return(pp)
}




###########################################################
#                       
#
#                 
########################## results ########################
o <- policy_iter(gamma=0.9, policy0=policy0)

library(plotly)
plot_ly(type = 'surface', x=~0:20, y=~0:20, z=~o$V_mat)



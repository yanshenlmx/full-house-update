#############
## apply park-factor adjustement to batting average, home run and hit
park_factor_year <- function(year){
  team_log_year <- team_log %>% filter(yearID == year)
  teamID <- unique(team_log_year$team.key)
  teamID <- teamID[!teamID %in% c('ALS', 'NLS', 'NL1', 'NL2', 'AL1', 'AL2')]
  do.call(rbind, mclapply(teamID, mc.cores = ncores, FUN = function(xx){
    home_RS <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 1) %>%
                     select(B_R))
    home_HS <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 1) %>%
                     select(B_H))
    home_HRS <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 1) %>%
                      select(B_HR))
    
    home_RA <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 1) %>%
                     select(P_R))
    home_HA <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 1) %>%
                     select(P_H))
    home_HRA <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 1) %>%
                      select(P_HR))
    
    away_RS <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 0) %>%
                     select(B_R))
    away_HS <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 0) %>%
                     select(B_H))
    away_HRS <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 0) %>%
                      select(B_HR))
    
    away_RA <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 0) %>%
                     select(P_R))
    away_HA <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 0) %>%
                     select(P_H))
    away_HRA <- sum(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 0) %>%
                      select(P_HR))
    
    homeG <- nrow(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 1))
    awayG <- nrow(team_log_year %>% filter(team.key == xx) %>% filter(team.alignment == 0))
    
    n <- nrow(unique(team_log_year %>% select(team.key)))
    
    park_index_R <- ((home_RS+home_RA)/(homeG)) / ((away_RS+away_RA)/(awayG))
    park_index_H <- ((home_HS+home_HA)/(homeG)) / ((away_HS+away_HA)/(awayG))
    park_index_HR <- ((home_HRS+home_HRA)/(homeG)) / ((away_HRS+away_HRA)/(awayG))
    
    park_factor_R <- ((park_index_R +1)/2)/((park_index_R+n-1)/n)
    park_factor_H <- ((park_index_H +1)/2)/((park_index_H+n-1)/n)
    park_factor_HR <- ((park_index_HR +1)/2)/((park_index_HR+n-1)/n)
    
    data.frame(teamID = xx, yearID = year, park_factor_R = park_factor_R, 
               park_factor_H = park_factor_H, park_factor_HR = park_factor_HR)
  }))
}

#############

#############
## moving window for park-factor
moving_window_mean <- function(x){
  n <- length(x)
  y <- rep(0,n)
  if (n == 1) {
    y[1] = x[1]
  }
  if (n == 2) {
    y[1:2] = mean(x)
  }
  if (n >= 3) {
    y[1] <- (x[1] + x[2])/2
    y[2:(n-1)] <- (x[1:(n-2)] + x[2:(n-1)] + x[3:n])/3
    y[n] <- (x[n-1] + x[n])/2
  }
  y
}
#############


#############

# establishes the interpolated empirical CDF
Ftilde <- function(y, t, ystar, component_name){
  y <- sort(y)
  n <- length(y)
  ytilde <- rep(0, n + 1)
  
  if (component_name == 'bWAR' | component_name == 'fWAR' | component_name == 'ERA') {
    ytilde[1] <- y[1] - (y[2] - y[1])
  }
  if (component_name == 'HR'| component_name == 'BB') {
    # since the minimal HR is greater or equal to 0.
    ytilde[1] <- 0
  }
  if (component_name == 'bWAR_p' | component_name == 'fWAR_p') {
    ytilde[1] <- y[1] - (y[2] - y[1])/10
  }
  if (component_name == 'SO' | component_name == 'AVG') {
    ytilde[1] <- ifelse(y[1] - (y[2] - y[1]) < 0, 0, y[1] - (y[2] - y[1]) )
  }
  
  ytilde[n+1] <- y[n] + ystar
  ytilde[2:n] <- unlist(lapply(2:n, function(j){
    (y[j]+y[j-1])/2 
  }))
  
  if (t >= ytilde[n+1]) {
    1 - 0.1^7
  } else if (t <= ytilde[1]) {
    0
  } else {
    j <- length(which(ytilde < t))
    (j - 1) / n + (t - ytilde[j]) / (n*(ytilde[j+1] - ytilde[j]))
  }
  
}

#############

#############

# computes the ystar in the empirical CDF for each season
thresh_fun <- function(component, component_name){
  ## stab means we add a small number to the largest value to avoid numerical stability problems. 
  ## stab depends on the scale of input values. In most cases, the default is 0.01. 
  ## cutoff means we select a certain percentage of systems that include maximal possible components in the tail
  ## rather than include k possible components in the tail based on the adjusted R adjusted square. 
  ## If the distance between the largest value and second largest value in the right tail is relatively large,
  ## adjusted R square may not be a good quantity to represent how well the fit is. 
  ## Then we usually choose the one sixth of all systems that their distances between the largest value and 
  ## second largest value are in the top one sixth. 
  
  if (component_name == 'bWAR') {
    #cutoff <- 1.5e-2 # raw comp
    # cutoff <- 1.17e-2 #BB
    cutoff <- 1.4e-2 # schell
    stab <- 0.01
  }
  if (component_name == 'fWAR') {
    #cutoff <- 1.75e-2
    cutoff <- 1.45e-2 # schell
    stab <- 0.01
  }
  if (component_name == 'HR') {
    cutoff <- 1.70e-2 # schell
    #cutoff <- 2.06e-2
    stab <- 0.01
  }
  if (component_name == 'BB') {
    #cutoff <- 4.05e-2
    cutoff <- 3.30e-2 # schell
    stab <- 0.027
  }
  
  if (component_name == 'AVG') {
    #cutoff <- 2.35e-2
    cutoff <- 2.30e-2 # schell
    stab <- 0.01
  }
  if (component_name == 'ERA') {
    # cutoff <- 0.47 # raw comp
    cutoff <- 0.4 # Schell
    stab <- 0.2
  }
  if (component_name == 'bWAR_p') {
    # cutoff <- 7.55e-3 # raw comp
    # cutoff <- 6e-3 # BB
    cutoff <- 7.09e-3 # Schell
    stab <- 0.01
  }
  if (component_name == 'fWAR_p') {
    #cutoff <- 5.7e-3 # raw comp
    cutoff <- 5.124e-3 # Schell
    stab <- 0.01
  }
  if (component_name == 'SO') {
    # cutoff <- 1.4 # raw comp
    cutoff <- 1.31 # Schell
    stab <- 1
  }
  # obtain initial quantities for linear approximation
  Y <- sort(as.matrix(component))
  n <- length(Y)
  Y[n] <- Y[n] + stab # for stability
  pi <- 1 - (n:1 - 1/3)/(n + 1/3)
  W <- log(pi/(1-pi))
  K1 = max(6, floor(1.3*sqrt(n))); K2 = 2*floor(log10(n)*sqrt(n))
  k <- 6
  
  # use arguments from Scholz section 3 for estimating k
  #
  # this argument is based on model fit and not longest stretch of 
  # contiguous I0
  ind <- NULL
  try({
    k_selector <- do.call(rbind, lapply(6:K2, function(k){
      
      Ytil <- Y - median(Y)
      Ztil <- tail(Ytil, k)
      M1k <- 1/(k-1) * sum( log(Ztil[2:k]/Ztil[1]) )
      M2k <- 1/(k-1) * sum( log(Ztil[2:k]/Ztil[1])^2 )
      ck <- M1k + 1 - 0.5*(1 - M1k^2/M2k)^{-1}
      fck <- ((-n*log(pi))^{-ck} - 1)/ck
      
      Sigma <- matrix(0, k, k)
      for(i in 1:k){
        for(j in 1:i){
          Sigma[i,j] <- i^{-ck-1} * j^{-ck}
        } 
      }
      for(j in 1:k){
        for(i in 1:(j-1)){
          Sigma[i,j] <- j^{-ck-1} * i^{-ck}
        } 
      }
      
      rotate <- function(x) t(apply(x, 2, rev))
      Sigma <- rotate(rotate(Sigma))
      Sigma.inv <-  solve(Sigma)
      eig <- eigen(Sigma.inv)
      C <- eig$vec %*% diag(sqrt(eig$val)) %*% t(eig$vec)
      Zk <- C %*% tail(Y, k)
      Xk <- cbind(1, tail(fck, k))
      Wk <-  C %*% Xk
      # try linear and quadratic model
      m1 <- lm(tail(Y, k) ~ tail(fck, k))
      m2 <- lm(tail(Y, k) ~ tail(fck, k) + I(tail(fck, k)^2))
      m3 <- lm(Zk ~ -1 + Wk)
      delta.sq <- summary(m3)$sigma^2
      Tk <- coef(m3)[2] / summary(m3)$sigma
      
      kappa.sq <- solve(crossprod(Wk))[2,2]
      kappa <- sqrt(kappa.sq)
      I0 <- c(kappa * qt(0.25, df = k - 2, ncp = 1/kappa),
              kappa * qt(0.75, df = k - 2, ncp = 1/kappa))
      I1 <- c(kappa * qt(0.05, df = k - 2, ncp = 1/kappa), 
              kappa * qt(0.95, df = k - 2, ncp = 1/kappa))
      I0int <- ifelse(I0[1] <= Tk && Tk <= I0[2], 1, 0)
      I1int <- ifelse(I1[1] <= Tk && Tk <= I1[2], 1, 0)
      c(k, Tk, I0int, I1int, summary(m1)$adj.r.squared, 
        summary(m2)$adj.r.squared)
      
    }))
    
    #k <- k_selector[max(which(k_selector[, 3] == 1)), 1]
    #k <- k_selector[which.max(k_selector[, 5]), 1]
    k_selector <- as.data.frame(k_selector)
    colnames(k_selector) <- c("k", "Tk", "I0", "I1", "R.sq", "Rquad.sq")
    k_selector_I0 <- k_selector %>% filter(I0 == 1)
    a <- which.max(k_selector_I0$R.sq)
    b <- which.max(k_selector_I0$Rquad.sq)
    ind <- which.max(c(k_selector_I0[a, ]$R.sq, 
                       k_selector_I0[b, ]$Rquad.sq))
    k <- k_selector_I0[c(a,b)[ind] , 1]
    if(diff(Y)[n-1] > cutoff){ 
      k <- max(k_selector_I0$k)
      if(k < 0) k <- K2
    }
    
  }, silent = TRUE)
  
  if(length(k) == 0) k <- round(mean(K1,K2))
  if(is.na(k)) k <- round(mean(K1,K2))
  if(k == 0) k <- round(mean(K1,K2))
  if(k >= n) k <- K2
  
  
  # find probability value using linear tail behavior
  Z <- tail(Y, k)
  m1 <- lm(tail(Y, k) ~ tail(pi, k))
  beta <- m1$coefficients
  ystar <- ub <- 0
  f <- function(x) beta[1] + beta[2] * x - max(Y)
  #delta <- beta[2]
  try({
    foo <- uniroot(f, c(0.0001, 5), tol = 1e-10)
    ub <- foo$root        
  })
  
  # find probability value using logistic tail behavior
  if(ub >= 1){
    m1 <- lm(tail(Y,k) ~ tail(W, k))
    beta <- m1$coefficients
    f <- function(x) beta[1] + beta[2] * log(x/(1-x)) - max(Y)
    try({
      foo <- uniroot(f, c(0.000001, 0.999999), tol = 1e-10)
      ub <- foo$root        
    })  
  }
  
  # if possible, find ystar by tying logistic behavior argument to 
  # our Ftilde function
  if(ub >= Ftilde(y = Y, t = max(Y), ystar = 10, component_name = component_name)){
    try({
      g <- function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar, component_name = component_name)
      bar <- uniroot(g, c(0, 10), tol = 1e-10)
      ystar <- bar$root
    })
  }
  
  # if the above is not possible, try a similar approach for different 
  # suitable values of k. 
  #
  # The above fails because ub < Ftilde(y = Y, t = max(Y), ystar = 10, component_name = component_name) 
  # suggesting that the largest achiever is performaing much worse than 
  # expected. Thus ystar should be "large". A default large value will 
  # be ystar = 6 (altered to be log(1 + 6) for stability). This will 
  # be used when all else fails.
  flag <- NULL
  if(ub < Ftilde(y = Y, t = max(Y), ystar = 10, component_name = component_name)){
    
    # first try for largest suitable k as dictated by Scholz Section 3
    k <- max(k_selector_I0$k) 
    m1 <- lm(tail(Y,k) ~ tail(W, k) + I(tail(W, k)^2))
    beta <- m1$coefficients
    f <- function(x) beta[1] + beta[2] * log(x/(1-x)) +
      beta[3] * log(x/(1-x))^2 - max(Y)
    flag <- try({
      foo <- uniroot(f, c(0.0001, 0.9999), tol = 1e-10)
      ub <- foo$root
    }, silent = TRUE)
    while(class(flag) == "try-error"){
      k <- k - 1
      # method fails; use ystar = 4
      if(k < 6){
        ystar <- 6
        break
      }
      m1 <- lm(tail(Y,k) ~ tail(W, k) + I(tail(W, k)^2))
      beta <- m1$coefficients
      f <- function(x) beta[1] + beta[2] * log(x/(1-x)) +
        beta[3] * log(x/(1-x))^2 - max(Y)
      flag <- try({
        foo <- uniroot(f, c(0.0001, 0.9999), tol = 1e-10)
        ub <- foo$root
      }, silent = TRUE)
    }
    
    ystar_1 <- NULL
    try({
      g <- function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar, component_name = component_name)
      bar <- uniroot(g, c(0, 10), tol = 1e-10)
      ystar_1 <- bar$root    	
    }, silent = TRUE)
    if(length(ystar_1) == 0) ystar_1 <- 6
    
    
    # now try for smallest suitable k as dictated by Scholz Section 3
    k <- min(k_selector_I0$k)
    if(length(k) == 0) k <- 6
    m1 <- lm(tail(Y,k) ~ tail(W, k) + I(tail(W, k)^2))
    beta <- m1$coefficients
    f <- function(x) beta[1] + beta[2] * log(x/(1-x)) + 
      beta[3] * log(x/(1-x))^2 - max(Y)
    flag <- try({
      foo <- uniroot(f, c(0.0001, 0.9999), tol = 1e-10)
      ub <- foo$root        
    }, silent = TRUE)  
    while(class(flag) == "try-error"){
      k <- k + 1
      # method fails; use ystar = 4
      if(k > max(k_selector_I0$k)){
        ystar <- 6
        break
      }
      m1 <- lm(tail(Y,k) ~ tail(W, k) + I(tail(W, k)^2))
      beta <- m1$coefficients
      f <- function(x) beta[1] + beta[2] * log(x/(1-x)) + 
        beta[3] * log(x/(1-x))^2 - max(Y)
      flag <- try({
        foo <- uniroot(f, c(0.0001, 0.9999), tol = 1e-10)
        ub <- foo$root        
      }, silent = TRUE)  
    }
    
    ystar_2 <- NULL
    try({
      g <- function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar, component_name = component_name)
      bar <- uniroot(g, c(0, 10), tol = 1e-10)
      ystar_2 <- bar$root			
    }, silent = TRUE)
    if(length(ystar_2) == 0) ystar_2 <- 6
    
    # take ystar as the average of the lowest working k and 
    # largest working k
    ystar <- mean(c(ystar_1, ystar_2))
    
  }
  
  # if changing k does not work, then try throwing out extreme 
  # observations and computing ystar for the reduced sample (Ytil)
  #
  # then compute ystar = max(Y) - max(Ytil) + ystar*
  #
  # where ystar* is computed with respect to Ytil
  if(ystar == 6){
    k <- k_selector_I0[c(a,b)[ind] , 1]
    if(diff(Y)[n-1] > cutoff){ 
      k <- max(k_selector_I0$k)
      if(k < 0) k <- K2
    }
    if(length(k) == 0) k <- round(mean(K1,K2))
    if(is.na(k)) k <- round(mean(K1,K2))
    if(k == 0) k <- round(mean(K1,K2))
    if(k >= n) k <- K2
    
    m1 <- lm(tail(Y, k) ~ tail(W, k) + I(tail(W, k)^2))
    beta <- m1$coefficients
    f <- function(x) beta[1] + beta[2] * log(x/(1-x)) + 
      beta[3] * log(x/(1-x))^2 - max(Y)
    flag <- flag2 <- try({
      foo <- uniroot(f, c(0.0001, 0.9999), tol = 1e-10)
      ub <- foo$root        
    }, silent = TRUE)
    
    n_lwr <- n 
    Ytil <- Y
    Xtil <- 1 - (n_lwr:1 - 1/3)/(n_lwr + 1/3)
    Wtil <- log(Xtil/(1-Xtil))
    while(class(flag) == "try-error" | class(flag2) == "try-error"){
      Ytil <- Ytil[-n_lwr]
      if(any(tail(Ytil, k) < 0)){
        ystar <- 6
        break
      }
      n_lwr <- n_lwr - 1
      Xtil <- 1 - (n_lwr:1 - 1/3)/(n_lwr + 1/3)
      Wtil <- log(Xtil/(1-Xtil))
      m2 <- lm(tail(Ytil, k) ~ tail(Wtil, k) + I(tail(Wtil, k)^2))
      beta <- m2$coefficients
      f <- function(x) beta[1] + beta[2] * log(x/(1-x)) + 
        beta[3] * log(x/(1-x))^2 - max(Ytil)
      flag <- try({
        foo <- uniroot(f, c(0.0001, 0.9999), tol = 1e-10)
        ub <- foo$root        
      }, silent = TRUE)
      flag2 <- try({
        g <- function(ystar) ub - Ftilde(y = Ytil, t = max(Ytil), ystar = ystar, component_name = component_name)
        bar <- uniroot(g, c(0, 10), tol = 1e-10)
        ystar <- bar$root
      }, silent = TRUE)
    }
    ystar <- max(Y) - Y[n_lwr] + ystar
    
  }
  
  # for stability
  ystar
  
}

#############

#############

# computes their latent talents using non-parametric distribution measuring the components
talent_computing_nonpara <- function(dataset, component_name, year, ystar, alpha){
  
  Ftilde <- function(y, t, ystar, component_name){
    y <- sort(y)
    n <- length(y)
    ytilde <- rep(0, n + 1)
    
    if (component_name == 'bWAR' | component_name == 'fWAR' | component_name == 'ERA') {
      ytilde[1] <- y[1] - (y[2] - y[1])
    }
    if (component_name == 'HR'| component_name == 'BB') {
      # since the minimal HR is greater or equal to 0.
      ytilde[1] <- 0
    }
    if (component_name == 'bWAR_p' | component_name == 'fWAR_p') {
      ytilde[1] <- y[1] - (y[2] - y[1])/10
    }
    if (component_name == 'SO' | component_name == 'AVG') {
      ytilde[1] <- ifelse(y[1] - (y[2] - y[1]) < 0, 0, y[1] - (y[2] - y[1]) )
    }
    
    ytilde[n+1] <- y[n] + ystar
    ytilde[2:n] <- unlist(lapply(2:n, function(j){
      (y[j]+y[j-1])/2
    }))
    
    if (t >= ytilde[n+1]) {
      1 - 0.1^7
    } else if (t <= ytilde[1]) {
      0
    } else {
      j <- length(which(ytilde < t))
      (j - 1) / n + (t - ytilde[j]) / (n*(ytilde[j+1] - ytilde[j]))
    }
    
  }
  
  Aptitude_nonpara <- function(p, alpha, npop){
    
    # converts order stats to their percentiles
    order_pbino <- function(p = 0, k = 1, n = 1e4){
      pbinom(k - 1, prob = p, size = n, lower.tail = FALSE)
    }
    
    # converts a vector of order stats 
    # to their percentiles. This vector should be the entire 
    # sample sorted in increasing order
    p <- sort(p) # just in case
    n <- length(p)
    u = unlist(lapply(1:n, function(j){
      #order_pbino(p[j], k = 251-n+j, n = 251)
      order_pbino(p[j], k = j, n = n)
    }))
    
    # transforms percentiles from order stats (in increasing order)
    # to Pareto values corresponding to the general population 
    # of a greater than or equal to size
    # default alpha is that of the Pareto principle 80-20
    n <- length(u)
    if(length(npop) == 1) npop <- rep(npop, n)
    unlist(lapply(1:n, function(j){
      qPareto(qbeta(u[j], j + npop[j] -n , n + 1 - j), t = 1, alpha = alpha)
      #qPareto(qbeta(u[j], j + npop[j] -n + 251-n , n + 1 - j), t = 1, alpha = alpha)
    }))
  }
  
  foo <- dataset %>% filter(yearID == year) %>% 
    arrange(comp) 
  bar <- foo %>% filter(full_time == 'Y')
  full_comp <- bar$comp
  ## batter WAR talent
  bar <- bar %>% 
    mutate(WAR_talent = 
             Aptitude_nonpara(p = unlist(lapply(comp, function(xx) 
               Ftilde(y = comp, t = xx, ystar = ystar, component_name = component_name))), alpha = alpha, npop = pops))
  
  max_WAR_talent <- max(bar$WAR_talent) - 1
  range <- which(!(foo$playerID %in% bar$playerID))
  
  ## using the distribution from full time players
  bar <- rbind(bar, do.call(rbind, lapply(range, function(j){
    rbind(bar %>% dplyr::select(-WAR_talent), foo[j, ]) %>% arrange(comp) %>%
      mutate(WAR_talent = Aptitude_nonpara(p = unlist(lapply(comp, function(xx) 
        Ftilde(y = full_comp, t = xx, ystar = ystar, component_name = component_name))), alpha = alpha, npop = pops)) %>%
      filter(full_time == 'N') %>% 
      mutate(WAR_talent = ifelse(WAR_talent > max_WAR_talent+1, max_WAR_talent, WAR_talent))
  })))
  bar %>% mutate(ystar = ystar)
}

#############

#############

# maps their latent talents to a common lab environment and get the era-adjusted statistics
# using non-parametric distribution measuring the components
career_talent_nonpara <- function(dataset, snippet, alpha, talent_new, pop_new, ytilde){
  Rev_Aptitude_nonpara <- function(x, ytilde, alpha, npop){
    
    # transforms ordered Pareto values corresponding to
    # the general population to percentiles from order stats
    # (in increasing order)
    n <- length(x)
    if(length(npop) == 1) npop <- rep(npop, n)
    u = unlist(lapply(1:n, function(j){
      pbeta(pPareto(x[j], t = 1, alpha = alpha), j + npop[j]-n, n + 1 - j)
    }))
    
    
    ## map the quantile to the a predicated sample value
    map_Y <- function(u, ytilde){
      n <- length(ytilde)-1
      seqence <- seq(0, 1, 1/n)
      pos <- findInterval(u, seqence)
      out <- (n*u -pos + 1) * (ytilde[(pos+1)] - ytilde[pos]) + ytilde[pos]
      return(out)
    }
    
    ## map the vector of quantiles to the predicated sample values
    n <- length(u)
    a <- qbeta(u, shape1 = 1:n, shape2 = n:1)
    out <- sapply(1:n, function(x) map_Y(a[x], ytilde = ytilde))
    out
    
  }
  
  talent_all <- sort(c(talent_new, snippet$WAR_talent))
  index_snippet <- max(which(talent_all == snippet$WAR_talent))
  
  snippet %>% 
    mutate(adj_comp = Rev_Aptitude_nonpara(talent_all, 
                                           ytilde = ytilde, alpha = alpha, 
                                           npop = pop_new)[index_snippet])
  
}

#############

#############
## get era-adjusted statistics
era_adjusted_comp <- function(component_name, dataset){
  batters_talent <- do.call(rbind, mclapply(years, mc.cores = ncores, function(yy){
    talent_computing_nonpara(dataset = dataset, 
                             component_name = component_name, 
                             year = yy, 
                             ystar = thresh_fun(component = dataset %>% 
                                                  filter(full_time == 'Y', yearID == yy) %>% 
                                                  select(comp), component_name = component_name), 
                             alpha = 1.16) })) %>% arrange(-WAR_talent)
  
  no_strike <- batters_talent %>% 
    filter(yearID < 1990, yearID >= 1977, yearID != 1981, 
           full_time == 'Y', lgID == 'NL') %>%
    arrange(WAR_talent)
  
  t <- isoreg(no_strike$WAR_talent, no_strike$comp)
  if (component_name == 'bWAR_p'| component_name == 'fWAR_p'| component_name == 'ERA'| component_name == 'SO') {
    talent_new <- quantile(no_strike$WAR_talent, probs = seq(0,154)/154)
  }
  if (component_name == 'bWAR'| component_name == 'fWAR'| component_name == 'HR' | component_name == 'BB'|component_name == 'AVG') {
    talent_new <- quantile(no_strike$WAR_talent, probs = (seq(0,250)/250))
  }
  
  comp_new <- as.stepfun(t)(talent_new)
  
  ystar <- thresh_fun(comp_new, component_name = component_name)
  pop_new <- round(mean(no_strike$pops))
  
  yy <- sort(comp_new)
  n <- length(yy)
  ytilde <- rep(0, n + 1)
  if (component_name == 'bWAR' | component_name == 'fWAR') {
    ytilde[1] <- yy[1] - (yy[2] - yy[1])
  }
  if (component_name == 'HR' | component_name == 'BB') {
    # since the minimal HR is greater or equal to 0.
    ytilde[1] <- 0
  }
  if (component_name == 'ERA') {
    ytilde[1] <- yy[1] - (yy[2] - yy[1])
  }
  if (component_name == 'bWAR_p'| component_name == 'fWAR_p') {
    ytilde[1] <- yy[1] - (yy[2] - yy[1])/10
  }
  if (component_name == 'SO' | component_name == 'AVG') {
    ytilde[1] <- ifelse(yy[1] - (yy[2] - yy[1]) < 0, 0, yy[1] - (yy[2] - yy[1]) )
  }
  ytilde[n+1] <- yy[n] + ystar
  ytilde[2:n] <- unlist(lapply(2:n, function(j){
    (yy[j]+yy[j-1])/2 
  }))
  
  career_kAB_1st <- do.call(rbind, mclapply(1:round(nrow(batters_talent)/3), function(zz){
    int <- career_talent_nonpara(dataset = batters_talent,  
                                 snippet = batters_talent[zz,], alpha = 1.16, 
                                 talent_new = talent_new, 
                                 pop_new = pop_new, ytilde = ytilde)
    int
  }, mc.cores = ncores))
  
  career_kAB_2nd <- do.call(rbind, mclapply((round(nrow(batters_talent)/3)+1):round(2*nrow(batters_talent)/3), function(zz){
    int <- career_talent_nonpara(dataset = batters_talent,  
                                 snippet = batters_talent[zz,], alpha = 1.16, 
                                 talent_new = talent_new, 
                                 pop_new = pop_new, ytilde = ytilde)
    int
  }, mc.cores = ncores)) 
  
  career_kAB_3rd <- do.call(rbind, mclapply((round(2*nrow(batters_talent)/3)+1):nrow(batters_talent), function(zz){
    int <- career_talent_nonpara(dataset = batters_talent, 
                                 snippet = batters_talent[zz,], alpha = 1.16, 
                                 talent_new = talent_new, 
                                 pop_new = pop_new, ytilde = ytilde)
    int
  }, mc.cores = ncores)) 
  
  career_kAB <- rbind(career_kAB_1st, career_kAB_2nd, career_kAB_3rd)
  
  return(list(era_adjusted = career_kAB, minimum = min(no_strike$comp)))
  
}

#############

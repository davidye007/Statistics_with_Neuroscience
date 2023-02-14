library(doParallel)
library(foreach)


load("3D_Activation_Object.RData")

parallel::detectCores()
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
print(my.cluster)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

start.time <- Sys.time()
test <- random_test_accelerated(Test_Day_12)
end.time <- Sys.time()
print(end.time - start.time)
parallel::stopCluster(cl = my.cluster)


start.time <- Sys.time()
test2 <- random_test(Test_Day_12)
end.time <- Sys.time()
print(end.time - start.time)


# Function: randomization distribution with t Statistic
#           where every time point is shuffled
random_test_accelerated <- function(object, tone = TRUE, test = FALSE) {
  neurons <- length(object[,1,1])
  trials <- length(object[1,,1])
  all_tests <- matrix(NA, nrow = neurons, ncol = trials)
  # randomization distribution with n = 1000 samples
  n <- 10
  foreach (i = 1:neurons) %dopar% {
    for (j in 1:trials) {
      rand_dist <- rep(0, n)
      if (tone) {
        pre_tone <- object[i,j,100:300]
        post_tone <- object[i,j,301:400]
        combined <- c(pre_tone,post_tone)
        foreach (k = 1:n, .combine = 'c') %dopar% {
          shuffled <- sample(combined)
          pre_rand <- shuffled[1:201]
          post_rand <- shuffled[202:301]
          rand_dist[k] <- as.numeric((t.test(x = post_rand,y = pre_rand,
                                             conf.level = 0.95)$statistic))
          
        }
        obs_stat <- as.numeric((t.test(x = post_tone,y = pre_tone,
                                       conf.level = 0.95)$statistic))
      }
      else {
        pre_food_shock <- object[i,j,301:429]
        post_food_shock <- object[i,j,430:550]
        combined <- c(pre_food_shock,post_food_shock)
        for (k in 1:n) {
          shuffled <- sample(combined)
          pre_rand <- shuffled[1:129]
          post_rand <- shuffled[130:250]
          rand_dist[k] <- as.numeric((t.test(x = post_rand,y = pre_rand,
                                             conf.level = 0.95)$statistic))
        }
        obs_stat <- as.numeric((t.test(x = post_food_shock,y = pre_food_shock,
                                       conf.level = 0.95)$statistic))
      }
      # lower p_higher means more significant increase in test statistic
      # lower p_lower means more significant decrease in test statistic
      p_higher <- sum(rand_dist >= obs_stat)/(n)
      p_lower <- sum(rand_dist <= obs_stat)/(n)
      # two-tailed thus multiply smaller p-value by 2
      all_tests[i,j] <- 2*(min(p_higher,p_lower))
    }
  }
  return (all_tests)
}


parallel::detectCores()
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
print(my.cluster)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

start.time <- Sys.time()
rand_dist2 <- foreach (k = 1:100000, .combine = 'c') %dopar% {
  shuffled <- sample(Food_Day_10[1,1,])
  pre_rand <- shuffled[1:201]
  post_rand <- shuffled[202:301]
  rand_dist <- as.numeric((t.test(x = post_rand,y = pre_rand,
                                  conf.level = 0.95)$statistic))
}
end.time <- Sys.time()
print(end.time - start.time)

parallel::stopCluster(cl = my.cluster)
start.time <- Sys.time()
rand_dist1 <- rep(0, 100000)
for (i in 1:100000) {
  shuffled <- sample(Food_Day_10[1,1,])
  pre_rand <- shuffled[1:201]
  post_rand <- shuffled[202:301]
  rand_dist1[i] <- as.numeric((t.test(x = post_rand,y = pre_rand,
                                  conf.level = 0.95)$statistic))
}
end.time <- Sys.time()
print(end.time - start.time)
























object <- Food_Day_10
object <- Food_Day_10[1:10,,]

parallel::detectCores()
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)


par_test <- foreach::foreach (i = 1:length(object[,1,1]), .combine=rbind) %dopar% {
  neurons <- length(object[,1,1])
  trials <- length(object[1,,1])
  neuron_tests <- rep(NA,trials)
  # randomization distribution with n = 1000 samples
  n <- 1000
  rand_dist <- rep(NA, n)
  for (j in 1:trials) {
    # if day 12, tone associated with food is trials 1,3,5,7,..,19
    #             tone associated with shock is trials 2,4,6,8,...,29
    if (TRUE) {
      combined <- object[i,j,100:400]
      for (k in 1:n) {
        shuffled <- sample(combined)
        rand_dist[k] <- (mean(shuffled[202:301])-mean(shuffled[1:201]))/
          (sqrt(((length(shuffled[202:301])-1)*var(shuffled[202:301])+(length(shuffled[1:201])-1)*var(shuffled[1:201]))/
                  (length(shuffled[202:301])+length(shuffled[1:201])-2))*
             sqrt(1/length(shuffled[202:301])+1/length(shuffled[1:201])))
      }
      obs_stat <- as.numeric((t.test(x = object[i,j,301:400],y = object[i,j,100:300],
                                     conf.level = 0.95)$statistic))
    }
    else {
      if(TRUE) {
        combined <- object[i,j,100:400]
        for (k in 1:n) {
          shuffled <- sample(combined)
          rand_dist[k] <- (mean(shuffled[202:301])-mean(shuffled[1:201]))/
            (sqrt(((length(shuffled[202:301])-1)*var(shuffled[202:301])+(length(shuffled[1:201])-1)*var(shuffled[1:201]))/
                    (length(shuffled[202:301])+length(shuffled[1:201])-2))*
               sqrt(1/length(shuffled[202:301])+1/length(shuffled[1:201])))
        }
        obs_stat <- as.numeric((t.test(x = object[i,j,301:400],y = object[i,j,100:300],
                                       conf.level = 0.95)$statistic))
      }
      else {
        combined <- object[i,j,301:550]
        for (k in 1:n) {
          shuffled <- sample(combined)
          rand_dist[k] <- (mean(shuffled[130:250])-mean(shuffled[1:129]))/
            (sqrt(((length(shuffled[130:250])-1)*var(shuffled[130:250])+(length(shuffled[1:129])-1)*var(shuffled[1:129]))/
                    (length(shuffled[130:250])+length(shuffled[1:129])-2))*
               sqrt(1/length(shuffled[130:250])+1/length(shuffled[1:129])))
        }
        obs_stat <- as.numeric((t.test(x = object[i,j,430:550],y = object[i,j,301:429],
                                       conf.level = 0.95)$statistic))
      }
    }
    # lower p_higher means more significant increase in test statistic
    # lower p_lower means more significant decrease in test statistic
    p_higher <- sum(rand_dist >= obs_stat)/(n)
    p_lower <- sum(rand_dist <= obs_stat)/(n)
    # two-tailed thus multiply smaller p-value by 2
    neuron_tests[j] <- 2*(min(p_higher,p_lower))
  }
  (neuron_tests)
}


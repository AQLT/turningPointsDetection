## Créer les poids/diagnostiques associés à la méthode DF pour h=6 et h=11
##
library(rjdfilters)
h_list <- c(9, 13, 23)
h_list <- (h_list-1)/2
h <- h_list[2]
# rjdfilters::dfa_filter
horizon = h
pdegree = 0
density = "uniform"
resolution = 11
targetfilter = lp_filter(horizon = horizon)$filters.coef[,horizon+1]
passband = 2*pi/12
library("future")
plan(multisession)
all_dfa_weights <- function(horizon=6, pdegree=2,
                            density = "uniform",
                            targetfilter = lp_filter(horizon = horizon)$filters.coef[,horizon+1],
                            passband = 2*pi/12,
                            resolution=101){
  data <- expand.grid(smoothness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000),
                      timeliness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000)
  )
  data$accuracy.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
  data <- data[(data$accuracy.weight<=1) & data$accuracy.weight>=0,]
  
  resultat_fs <- lapply(seq_len(nrow(data)),function(i){
    print(i)
    future({
      x = data$accuracy.weight[i]
      y = data$smoothness.weight[i]
      z = data$timeliness.weight[i]
      tryCatch({
        filter <- dfa_filter(horizon = horizon,
                             accuracy.weight = x,
                             smoothness.weight=y,
                             timeliness.weight=z,
                             passband=passband,
                             targetfilter = targetfilter)
        
        filter$filters.coef
      }, error = function(e) NULL)
    })
  })
  resultat <- lapply(resultat_fs, value)
  # remove_i <- !sapply(resultat, is.null)
  list(weights = data, results = resultat)
}
for (h in h_list){
  for(degree in 0:3){
    if(file.exists(
      sprintf("filters/dfa/dfa_pdegree%i_h%i.RDS",
              degree, h)))
      next;
    print(sprintf("h=%i, pdegree = %i", h, degree))
    res = all_dfa_weights(horizon = h,pdegree = degree,
                          resolution = 21)
    saveRDS(res,
            sprintf("filters/dfa/dfa_pdegree%i_h%i.RDS",
                    degree, h))
  }
}


for(degree in 0:3){
  h4 <- readRDS(sprintf("filters/dfa/dfa_pdegree%i_h%i.RDS",
                        degree, 4))
  h6 <- readRDS(sprintf("filters/dfa/dfa_pdegree%i_h%i.RDS",
                        degree, 6))
  h11 <- readRDS(sprintf("filters/dfa/dfa_pdegree%i_h%i.RDS",
                         degree, 11))
  weights = h6$weights
  total <- lapply(seq_along(h6$results), function(i){
    res = list(h4$results[[i]], h6$results[[i]], h11$results[[i]])
    names(res) <- c("h=4", "h=6","h=11")
    res
  })
  w_to_keep <- !sapply(total, function(x) any(sapply(x, function(y) is.null(y))))
  
  saveRDS(list(weights = weights[w_to_keep,],
               coefs = total[w_to_keep]),
          sprintf("filters/dfa_pdegree%i.RDS",
                  degree))
}



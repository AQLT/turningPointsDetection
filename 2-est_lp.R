if(!dir.exists("results"))
  dir.create("results")
if(!dir.exists("results/lp"))
  dir.create("results/lp")
library(rjdfilters)
library(AQLThesis)
library("future")
plan(multisession)

list_series <- list.files("data/byseries", full.names = TRUE)
list_kernel <- c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube",
                 "Gaussian", "Triangular", "Parabolic")
kernel = "Henderson"
method = "LC"
s = list_series[1]
for(kernel in list_kernel){
  print(kernel)
  for (method in c("LC","QL", "CQ", "DAF")){
    print(method)
    for(s in list_series){
      name_file <- gsub(".RDS$", "", basename(s))
      print(name_file)
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results/lp/%s_%s_%s.RDS",
                         name_file,
                         tolower(kernel), tolower(method))
      if(file.exists(nom_f_s))
        next;
      
      series_s <- lapply(names(data), function(nom_d){
        x <- data[[nom_d]]
        l = data_info[[nom_d]]["optimal_length"]
        icr = data_info[[nom_d]][sprintf("icr-%s", l)]
        lp_coef = lp_filter(horizon = (l-1)/2,
                            kernel = kernel,
                            endpoints = method,
                            ic = icr)$filters.coef
        jfilter(x, lp_coef)
      })
      names(series_s) <- names(data)
      
      # saveRDS(series_s, nom_f_s)
      
      print("turning points")
      tp <- lapply(series_s, turning_points)
      saveRDS(tp,
              sprintf("results/lp/%s_%s_%s_tp.RDS",
                      name_file,
                      tolower(kernel), tolower(method)))
      
      revisions_firstest <- first_est_revisions(series_s)
      revisions_consest <- consecutive_est_revisions(series_s)
      
      saveRDS(revisions_firstest,
              sprintf("results/lp/%s_%s_fe_rev.RDS",
                      name_file,
                      tolower(method)))
      saveRDS(revisions_consest,
              sprintf("results/lp/%s_%s_ce_rev.RDS",
                      name_file,
                      tolower(method)))
    }
  }
}


fs <- list()
j <- 1
reload <- FALSE
for(kernel in list_kernel){
  print(kernel)
  for (method in c("LC","QL", "CQ", "DAF")){
    print(method)
    for(s in list_series){
      name_file <- gsub(".RDS$", "", basename(s))
      print(name_file)
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results/lp/%s_%s_%s.RDS",
                         name_file,
                         tolower(kernel), tolower(method))
      if(file.exists(nom_f_s))
        next;
      
      reload <- TRUE
      fs[[j]] <- future({
      series_s <- lapply(names(data), function(nom_d){
        x <- data[[nom_d]]
        l = data_info[[nom_d]]["optimal_length"]
        icr = data_info[[nom_d]][sprintf("icr-%s", l)]
        lp_coef = lp_filter(horizon = (l-1)/2,
                            kernel = kernel,
                            endpoints = method,
                            ic = icr)$filters.coef
        jfilter(x, lp_coef)
      })
      names(series_s) <- names(data)
      
      # saveRDS(series_s, nom_f_s)
      
      print("turning points")
      tp <- lapply(series_s, turning_points)
      saveRDS(tp,
              sprintf("results/lp/%s_%s_%s_tp.RDS",
                      name_file,
                      tolower(kernel), tolower(method)))
      
      revisions_firstest <- first_est_revisions(series_s)
      revisions_consest <- consecutive_est_revisions(series_s)
      
      saveRDS(revisions_firstest,
              sprintf("results/lp/%s_%s_fe_rev.RDS",
                      name_file,
                      tolower(method)))
      saveRDS(revisions_consest,
              sprintf("results/lp/%s_%s_ce_rev.RDS",
                      name_file,
                      tolower(method)))
      })
      j <- j+1
    }
    if(reload){
      vs <- lapply(fs, value)
      fs <- list()
      j <- 1
    }
  }
}

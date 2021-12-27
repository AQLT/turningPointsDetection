if(!dir.exists("results"))
  dir.create("results")
if(!dir.exists("results/rkhs"))
  dir.create("results/rkhs")
rkhs_f <- readRDS("filters/rkhs_rw_p3.RDS")
library(rjdfilters)
library(AQLThesis)
library("future")
plan(multisession)

list_series <- list.files("data/byseries", full.names = TRUE)
list_method <- c("phase", "frf", "smoothness", "accuracy")
s = list_series[1]
method = "phase"
# for(method in list_method){
#   print(method)
#     for(s in list_series){
#       name_file <- gsub(".RDS$", "", basename(s))
#       print(name_file)
#       data <- readRDS(s)
#       data_info <- readRDS(sub("byseries", "byseriesinfo", s))
#       nom_f_s <- sprintf("results/rkhs/%s_%s.RDS",
#                          name_file,
#                          tolower(method))
#       if(file.exists(nom_f_s))
#         next;
#       
#       series_s <- lapply(names(data), function(nom_d){
#         x <- data[[nom_d]]
#         l = data_info[[nom_d]]["optimal_length"]
#         coef = rkhs_f[[sprintf("h=%i", (l-1)/2)]][[method]]
#         jfilter(x, coef)
#       })
#       names(series_s) <- names(data)
#       
#       saveRDS(series_s, nom_f_s)
#       
#       print("turning points")
#       tp <- lapply(series_s, turning_points)
#       saveRDS(tp,
#               sprintf("results/rkhs/%s_%s_tp.RDS",
#                       name_file,
#                       tolower(method)))
#     }
# }

# Version plus rapide avec future :
library("future")
plan(multisession)
fs <- list()
i <- 0
for(method in list_method){
  print(method)
  for(s in list_series){
    i <- i+1
    name_file <- gsub(".RDS$", "", basename(s))
    print(name_file)
    fs[[i]] <- future({
      
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results/rkhs/%s_%s.RDS",
                         name_file,
                         tolower(method))
      if(file.exists(nom_f_s))
        next;
      
      series_s <- lapply(names(data), function(nom_d){
        x <- data[[nom_d]]
        l = data_info[[nom_d]]["optimal_length"]
        coef = rkhs_f[[sprintf("h=%i", (l-1)/2)]][[method]]
        jfilter(x, coef)
      })
      names(series_s) <- names(data)
      
      # saveRDS(series_s, nom_f_s)
      
      print("turning points")
      tp <- lapply(series_s, turning_points)

      saveRDS(tp,
              sprintf("results/rkhs/%s_%s_tp.RDS",
                      name_file,
                      tolower(method)))
      revisions_firstest <- first_est_revisions(series_s)
      revisions_consest <- consecutive_est_revisions(series_s)

      saveRDS(revisions_firstest,
              sprintf("results/rkhs/%s_%s_fe_rev.RDS",
                      name_file,
                      tolower(method)))
      saveRDS(revisions_consest,
              sprintf("results/rkhs/%s_%s_ce_rev.RDS",
                      name_file,
                      tolower(method)))
      # last_est <- series_s[[length(series_s)]]
      # 
      # revisions_firstest <- t(sapply(series_s, function(est){
      #   if(is.null(est)){
      #     return(rep(NA,11))
      #   }
      #   tail(abs((est-last_est)/last_est),11)[11:1]
      # }))
      # revisions_firstest <- ts(revisions_firstest, start = first_est, frequency = 12)
      # 
      # revisions_consest <- t(sapply(seq_along(series_s), function(j){
      #   if(j == length(series_s) || 
      #      is.null(series_s[[j]]) || 
      #      is.null(series_s[[j+1]])){
      #     return(rep(NA,11))
      #   }
      #   tail(abs((series_s[[j]]-series_s[[j+1]])/series_s[[j+1]]),11)[11:1]
      # }))
      # 
      # colnames(revisions_firstest) <- 
      #   colnames(revisions_consest) <- 
      #   sprintf("rev q%i",0:10)
      # rownames(revisions_consest) <- rownames(revisions_firstest)
      TRUE
    })
  }
}

vs <- lapply(fs, value)

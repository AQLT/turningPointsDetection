if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_nber"))
  dir.create("results_nber")
if(!dir.exists("results_simul/localic_lp"))
  dir.create("results_simul/localic_lp")
if(!dir.exists("results_nber/localic_lp"))
  dir.create("results_nber/localic_lp")
library(rjdfilters)
library(AQLThesis)
library("future")
plan(multisession)

kernel = "Henderson"
method = "LC"
s = list_series[1]
fs <- list()
j <- 1
reload <- FALSE
for (method in c("LC","QL")){
  print(method)
  for(s in list.files("data_simul/byseries", full.names = TRUE)){
    for(d in 2:3){
      for(h in 3:6) {
        name_file <- gsub(".RDS$", "", basename(s))
        print(name_file)
        data <- readRDS(s)
        if(method == "LC"){
          complement = sprintf("_d%i", d)
        } else {
          complement = ""
        }
        data_info <- readRDS(sprintf("data_simul/byseriespente/%s_h%i.RDS",
                                     gsub(".RDS", "",basename(s)),h))
        
        nom_f_s <- sprintf("results_simul/localic_lp/%s_%s_h%i%s.RDS",
                           name_file, tolower(method), h, complement)
        nom_f_s_tp <- 
          sprintf("results_simul/localic_lp/%s_%s_h%i%s_tp.RDS",
                  name_file,
                  tolower(method), h, complement)
        
        nom_f_s_rev_fe <- sprintf("results_simul/localic_lp/%s_%_h%is%s_fe_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        nom_f_s_rev_ce <- sprintf("results_simul/localic_lp/%s_%s_h%i%s_ce_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        
        if(all(file.exists(nom_f_s_tp),
               file.exists(nom_f_s_rev_fe),
               file.exists(nom_f_s_rev_ce)))
          next;
        
        reload <- TRUE
        fs[[j]] <- future({
          print(s)
          series_s <- lapply(names(data), function(nom_d){
            x <- data[[nom_d]]
            data_t = data_info[[nom_d]]
            l = 13
            if(method == "LC"){
              delta = data_t[sprintf("d=%i", d)]
            } else {
              delta = data_t["deriv2"]
            }
            ratio = delta / sqrt(data_t["sigma2"])
            icr = 2/(sqrt(pi) * ratio)
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
                  nom_f_s_tp)
          
          revisions_firstest <- first_est_revisions(series_s)
          revisions_consest <- consecutive_est_revisions(series_s)
          
          saveRDS(revisions_firstest,
                  nom_f_s_rev_fe)
          saveRDS(revisions_consest,
                  nom_f_s_rev_ce)
          TRUE
        })
        j <- j+1 
      }
      
    }
  }
}
if(reload){
  vs <- lapply(fs, value)
  fs <- list()
  j <- 1
}


for (method in c("LC","QL")){
  print(method)
  for(s in list.files("data/byseries", full.names = TRUE)){
    for(d in 2:3){
      for(h in 3:6) {
        name_file <- gsub(".RDS$", "", basename(s))
        print(name_file)
        data <- readRDS(s)
        if(method == "LC"){
          complement = sprintf("_d%i", d)
        } else {
          complement = ""
        }
        data_info <- readRDS(sprintf("data/byseriespente/%s_h%i.RDS",
                                     gsub(".RDS", "",basename(s)),h))
        
        nom_f_s <- sprintf("results_nber/localic_lp/%s_%s_h%i%s.RDS",
                           name_file, tolower(method), h, complement)
        nom_f_s_tp <- 
          sprintf("results_nber/localic_lp/%s_%s_h%i%s_tp.RDS",
                  name_file,
                  tolower(method), h, complement)
        
        nom_f_s_rev_fe <- sprintf("results_nber/localic_lp/%s_%_h%is%s_fe_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        nom_f_s_rev_ce <- sprintf("results_nber/localic_lp/%s_%s_h%i%s_ce_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        
        if(all(file.exists(nom_f_s_tp),
               file.exists(nom_f_s_rev_fe),
               file.exists(nom_f_s_rev_ce)))
          next;
        
        reload <- TRUE
        fs[[j]] <- future({
          print(s)
          series_s <- lapply(names(data), function(nom_d){
            x <- data[[nom_d]]
            data_t = data_info[[nom_d]]
            l = 13
            if(method == "LC"){
              delta = data_t[sprintf("d=%i", d)]
            } else {
              delta = data_t["deriv2"]
            }
            ratio = delta / sqrt(data_t["sigma2"])
            icr = 2/(sqrt(pi) * ratio)
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
                  nom_f_s_tp)
          
          revisions_firstest <- first_est_revisions(series_s)
          revisions_consest <- consecutive_est_revisions(series_s)
          
          saveRDS(revisions_firstest,
                  nom_f_s_rev_fe)
          saveRDS(revisions_consest,
                  nom_f_s_rev_ce)
          TRUE
        })
        j <- j+1 
      }
      
    }
  }
}
if(reload){
  vs <- lapply(fs, value)
  fs <- list()
  j <- 1
}

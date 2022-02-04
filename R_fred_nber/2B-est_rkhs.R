if(!dir.exists("results_nber"))
  dir.create("results_nber")
if(!dir.exists("results_nber/rkhs"))
  dir.create("results_nber/rkhs")
rkhs_f <- readRDS("filters/rkhs_rw_p3.RDS")
library(rjdfilters)
library(AQLThesis)
library("future")
plan(multisession)

list_series <- list.files("data/byseries_nber", full.names = TRUE)
list_method <- names(rkhs_f$`h=6`)
s = list_series[1]
method = "phase"
fs <- list()
i <- 0
for(method in list_method){
  print(method)
  for(s in list_series){
    name_file <- gsub(".RDS$", "", basename(s))
    print(name_file)
    
    
    nom_f_s <- sprintf("results_nber/rkhs/%s_%s.RDS",
                       name_file,
                       tolower(method))
    nom_f_s_tp <- 
      sprintf("results_nber/rkhs/%s_%s_tp.RDS",
              name_file,
              tolower(method))
    
    nom_f_s_rev_fe <- sprintf("results_nber/rkhs/%s_%s_fe_rev.RDS",
                              name_file,
                              tolower(method))
    nom_f_s_rev_ce <- sprintf("results_nber/rkhs/%s_%s_ce_rev.RDS",
                              name_file,
                              tolower(method))

    if(all(file.exists(nom_f_s_tp),
           file.exists(nom_f_s_rev_fe),
           file.exists(nom_f_s_rev_ce)))
      next;
    
    i <- i+1
    
    fs[[i]] <- future({
      print(s)
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries_nber", "byseriesinfo_nber", s))

      series_s <- lapply(names(data), function(nom_d){
        x <- data[[nom_d]]
        l = data_info[[nom_d]]["optimal_length"]
        l = 13
        coef = rkhs_f[[sprintf("h=%i", (l-1)/2)]][[method]]
        jfilter(x, coef)
      })
      names(series_s) <- names(data)
      
      # saveRDS(series_s, nom_f_s)
      
      print("turning points")
      tp <- lapply(series_s, turning_points)

      saveRDS(tp, nom_f_s_tp)
      revisions_firstest <- first_est_revisions(series_s)
      revisions_consest <- consecutive_est_revisions(series_s)
      
      saveRDS(revisions_firstest,
              nom_f_s_rev_fe)
      saveRDS(revisions_consest,
              nom_f_s_rev_ce)
      TRUE
    })
  }
}

vs <- lapply(fs, value)

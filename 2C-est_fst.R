if(!dir.exists("results"))
  dir.create("results")
if(!dir.exists("results/fst"))
  dir.create("results/fst")
library(rjdfilters)
library(AQLThesis)
library("future")
plan(multisession)
list_series <- list.files("data/byseries", full.names = TRUE)
list_degree <- c(0:3)
s = list_series[1]
q = 0
fs <- list()
j <- 1
reload <- FALSE

for(degree in list_degree){
  print(degree)
  all_coefs <- readRDS(sprintf("filters/fst_pdegree%i.RDS",degree))
  weights = all_coefs$weights
  all_coefs = all_coefs$coefs
  for(i in seq_along(all_coefs)){
    print(i)
    for(s in list_series){
      name_file <- gsub(".RDS$", "", basename(s))
      print(name_file)
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results/fst/%s_degree%s_weight%i.RDS",
                         name_file,
                         degree,
                         i
      )
      nom_f_s_tp <- sprintf("results/fst/%s_degree%s_weight%i_tp.RDS",
                            name_file,
                            degree,
                            i
      )
      
      nom_f_s_rev_fe <- sprintf("results/fst/%s_degree%s_weight%i_fe_rev.RDS",
                                name_file,
                                degree,
                                i
      )
      nom_f_s_rev_ce <- 
        sprintf("results/fst/%s_degree%s_weight%i_ce_rev.RDS",
                name_file,
                degree,
                i
        )
      
      coefs = all_coefs[[i]]
      if(all(file.exists(nom_f_s_tp),
             file.exists(nom_f_s_rev_fe),
             file.exists(nom_f_s_rev_ce)))
        next;
      reload <- TRUE
      fs[[j]] <- future({
        print(s)
        series_s <- lapply(names(data), function(nom_d){
          x <- data[[nom_d]]
          l = data_info[[nom_d]]["optimal_length"]
          if(isTRUE(all.equal(as.numeric(l),9)))
            return(NULL)
          coef = coefs[[sprintf("h=%i", (l-1)/2)]]
          jfilter(x, coef)
        })
        names(series_s) <- names(data)
        
        # saveRDS(series_s, nom_f_s)
        
        print("turning points")
        tp <- lapply(series_s, turning_points)
        saveRDS(tp,
                nom_f_s_tp
        )
        
        revisions_firstest <- first_est_revisions(series_s)
        revisions_consest <- consecutive_est_revisions(series_s)
        
        saveRDS(revisions_firstest, nom_f_s_rev_fe)
        saveRDS(revisions_consest, nom_f_s_rev_ce)
        TRUE
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
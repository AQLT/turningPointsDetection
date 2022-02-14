library(AQLThesis)
if(!dir.exists("results_nber/compile_revisions"))
  dir.create("results_nber/compile_revisions")

suffix = "_fe_rev"
for(suffix in c("_fe_rev", "_ce_rev")){
  
  all_files <- list.files("results_nber/lp/",pattern = suffix,full.names = TRUE)
  
  all_rev <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    data = readRDS(f)
    data = summary_revisions(data,                    
                             peaks = nber_tp_m[,"Peak"],
                             troughs = nber_tp_m[,"Trough"])
    
    full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
    split <- strsplit(full_names, "_")
    series <- sapply(split, `[`, 1)
    kernel <- sapply(split, `[`, 2)
    method <- sapply(split, `[`, 3)
    data$series <- series
    data$kernel <- kernel
    data$method <- method
    data
  })
  all_rev = do.call(rbind, all_rev)
  
  saveRDS(all_rev, sprintf("results_nber/compile_revisions/lp%s.RDS", suffix))
  
  
  all_files <- list.files("results_nber/rkhs/",pattern = suffix,full.names = TRUE)
  
  all_rev <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    
    data = readRDS(f)
    data = summary_revisions(data,                    
                             peaks =  nber_tp_m[,"Peak"],
                             troughs = nber_tp_m[,"Trough"])
    
    full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
    split <- strsplit(full_names, "_")
    series <- sapply(split, `[`, 1)
    method <- sapply(split, `[`, 2)
    data$series <- series
    data$kernel <- "Henderson"
    data$method <- method
    data
  })
  all_rev = do.call(rbind, all_rev)
  
  saveRDS(all_rev, sprintf("results_nber/compile_revisions/rkhs%s.RDS", suffix))
  
  
  for(degree in 0:3){
    all_files <- list.files(sprintf("results_nber/dfa/dfa%i", degree),
                            pattern = suffix,full.names = TRUE)
    
    all_rev <- lapply(seq_along(all_files), function(i){
      print(i)
      f = all_files[i]
      data = readRDS(f)
      if(is.null(data))
        return(data)
      data = summary_revisions(data,                    
                               peaks =  nber_tp_m[,"Peak"],
                               troughs = nber_tp_m[,"Trough"])
      
      full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
      split <- strsplit(full_names, "_")
      series <- sapply(split, `[`, 1)
      weight <- sapply(split, `[`, 3)
      data$series <- series
      data$degree <- degree
      data$weight <- weight
      data
    })
    all_rev = do.call(rbind, all_rev)
    
    saveRDS(all_rev, sprintf("results_nber/compile_revisions/dfa%i%s.RDS", degree, suffix))
  }
  saveRDS(do.call(rbind,
                  lapply(sprintf("results_nber/compile_revisions/dfa%i%s.RDS", 0:3, suffix),
                         readRDS)),
          sprintf("results_nber/compile_revisions/dfa%s.RDS",suffix))
  file.remove(sprintf("results_nber/compile_revisions/dfa%i%s.RDS", 0:3, suffix))
  
  for(degree in 0:3){
    all_files <- list.files(sprintf("results_nber/fst/fst%i", degree),
                            pattern = suffix,full.names = TRUE)
    
    all_rev <- lapply(seq_along(all_files), function(i){
      print(i)
      f = all_files[i]
      data = readRDS(f)
      if(is.null(data))
        return(data)
      data = summary_revisions(data,                    
                               peaks =  nber_tp_m[,"Peak"],
                               troughs = nber_tp_m[,"Trough"])
      
      full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
      split <- strsplit(full_names, "_")
      series <- sapply(split, `[`, 1)
      weight <- sapply(split, `[`, 3)
      data$series <- series
      data$degree <- degree
      data$weight <- weight
      data
    })
    all_rev = do.call(rbind, all_rev)
    
    saveRDS(all_rev, sprintf("results_nber/compile_revisions/fst%i%s.RDS", degree, suffix))
  }
  
  saveRDS(do.call(rbind,
                  lapply(sprintf("results_nber/compile_revisions/fst%i%s.RDS", 0:3, suffix),
                         readRDS)),
          sprintf("results_nber/compile_revisions/fst%s.RDS",suffix))
  file.remove(sprintf("results_nber/compile_revisions/fst%i%s.RDS", 0:3, suffix))
}



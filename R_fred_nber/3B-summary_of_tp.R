library(AQLThesis)
if(!dir.exists("results_nber/compile_tp"))
  dir.create("results_nber/compile_tp")


all_files <- list.files("results_nber/lp/",pattern = "_tp",full.names = TRUE)

all_tp_lp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"])
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
kernel <- sapply(split, `[`, 2)
method <- sapply(split, `[`, 3)
all_t <- data.frame(t(sapply(all_tp_lp,`[[`,"troughs")),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_lp,`[[`,"peaks")),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names


saveRDS(all_t, "results_nber/compile_tp/troughs_lp.RDS")
saveRDS(all_p, "results_nber/compile_tp/peaks_lp.RDS")



all_files <- list.files("results_nber/rkhs/",pattern = "_tp",full.names = TRUE)

all_tp_rkhs <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"])
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
method <- sapply(split, `[`, 2)
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp_rkhs,`[[`,"troughs")),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_rkhs,`[[`,"peaks")),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_nber/compile_tp/troughs_rkhs.RDS")
saveRDS(all_p, "results_nber/compile_tp/peaks_rkhs.RDS")


all_files <- list.files("results_nber/arima/",pattern = "_tp",full.names = TRUE)

all_tp_arima <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"])
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
method <- "auto_arima"
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp_arima,`[[`,"troughs")),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_arima,`[[`,"peaks")),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_nber/compile_tp/troughs_arima.RDS")
saveRDS(all_p, "results_nber/compile_tp/peaks_arima.RDS")

for(degree in 0:3){
  all_files <- list.files(sprintf("results_nber/dfa/dfa%i", degree),
                          pattern = "_tp",full.names = TRUE)
  
  
  all_tp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),                    
                     peaks = nber_tp_m[,"Peak"],
                     troughs = nber_tp_m[,"Trough"])
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  weight <- sapply(split, `[`, 3)
  all_t <- data.frame(t(sapply(all_tp,`[[`,"troughs")),
                      series, degree, weight)
  all_p <- data.frame(t(sapply(all_tp,`[[`,"peaks")),
                      series, degree, weight)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_nber/compile_tp/troughs_dfa%i.RDS", degree))
  saveRDS(all_p, sprintf("results_nber/compile_tp/peaks_dfa%i.RDS", degree))
}
saveRDS(do.call(rbind,
                lapply(sprintf("results_nber/compile_tp/troughs_dfa%i.RDS", 0:3),
                       readRDS)),
        "results_nber/compile_tp/troughs_dfa.RDS")
saveRDS(do.call(rbind,
                lapply(sprintf("results_nber/compile_tp/peaks_dfa%i.RDS", 0:3),
                       readRDS)),
        "results_nber/compile_tp/peaks_dfa.RDS")
file.remove(c(sprintf("results_nber/compile_tp/troughs_dfa%i.RDS", 0:3),
              sprintf("results_nber/compile_tp/peaks_dfa%i.RDS", 0:3)))

for(degree in 0:3){
  all_files <- list.files(sprintf("results_nber/fst/fst%i", degree),
                          pattern = "_tp",full.names = TRUE)
  
  all_tp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),                    
                     peaks = nber_tp_m[,"Peak"],
                     troughs = nber_tp_m[,"Trough"])
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  weight <- sapply(split, `[`, 3)
  all_t <- data.frame(t(sapply(all_tp,`[[`,"troughs")),
                      series, degree, weight)
  all_p <- data.frame(t(sapply(all_tp,`[[`,"peaks")),
                      series, degree, weight)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_nber/compile_tp/troughs_fst%i.RDS", degree))
  saveRDS(all_p, sprintf("results_nber/compile_tp/peaks_fst%i.RDS", degree))
}

saveRDS(do.call(rbind,
                lapply(sprintf("results_nber/compile_tp/troughs_fst%i.RDS", 0:3),
                       readRDS)),
        "results_nber/compile_tp/troughs_fst.RDS")
saveRDS(do.call(rbind,
                lapply(sprintf("results_nber/compile_tp/peaks_fst%i.RDS", 0:3),
                       readRDS)),
        "results_nber/compile_tp/peaks_fst.RDS")
file.remove(c(sprintf("results_nber/compile_tp/troughs_fst%i.RDS", 0:3),
              sprintf("results_nber/compile_tp/peaks_fst%i.RDS", 0:3)))

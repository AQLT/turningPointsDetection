library(AQLThesis)
if(!dir.exists("results/compile_tp"))
  dir.create("results/compile_tp")

series_with_tp <- readRDS("results/series_to_study.RDS")

all_files <- list.files("results/lp/",pattern = "_tp",full.names = TRUE)
all_files <- grep(paste(sprintf("(%s)",series_with_tp), collapse = "|"),
                  all_files, value = TRUE)


all_tp_lp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f))
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


saveRDS(all_t, "results/compile_tp/troughs_lp.RDS")
saveRDS(all_p, "results/compile_tp/peaks_lp.RDS")



all_files <- list.files("results/rkhs/",pattern = "_tp",full.names = TRUE)
all_files <- grep(paste(sprintf("(%s)",series_with_tp), collapse = "|"),
                  all_files, value = TRUE)

all_tp_rkhs <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f))
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

saveRDS(all_t, "results/compile_tp/troughs_rkhs.RDS")
saveRDS(all_p, "results/compile_tp/peaks_rkhs.RDS")

for(degree in 0:3){
  all_files <- list.files(sprintf("results/dfa/dfa%i", degree),
                          pattern = "_tp",full.names = TRUE)
  all_files <- grep(paste(sprintf("(%s)",series_with_tp), collapse = "|"),
                    all_files, value = TRUE)
  
  all_tp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f))
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
  
  saveRDS(all_t, sprintf("results/compile_tp/troughs_dfa%i.RDS", degree))
  saveRDS(all_p, sprintf("results/compile_tp/peaks_dfa%i.RDS", degree))
}
saveRDS(do.call(rbind,
                lapply(sprintf("results/compile_tp/troughs_dfa%i.RDS", 0:3),
                       readRDS)),
        "results/compile_tp/troughs_dfa.RDS")
saveRDS(do.call(rbind,
                lapply(sprintf("results/compile_tp/peaks_dfa%i.RDS", 0:3),
                       readRDS)),
        "results/compile_tp/peaks_dfa.RDS")


for(degree in 0:3){
  all_files <- list.files(sprintf("results/fst/fst%i", degree),
                          pattern = "_tp",full.names = TRUE)
  all_files <- grep(paste(sprintf("(%s)",series_with_tp), collapse = "|"),
                    all_files, value = TRUE)
  
  all_tp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f))
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
  
  saveRDS(all_t, sprintf("results/compile_tp/troughs_fst%i.RDS", degree))
  saveRDS(all_p, sprintf("results/compile_tp/peaks_fst%i.RDS", degree))
}

saveRDS(do.call(rbind,
                lapply(sprintf("results/compile_tp/troughs_fst%i.RDS", 0:3),
                       readRDS)),
        "results/compile_tp/troughs_fst.RDS")
saveRDS(do.call(rbind,
                lapply(sprintf("results/compile_tp/peaks_fst%i.RDS", 0:3),
                       readRDS)),
        "results/compile_tp/peaks_fst.RDS")

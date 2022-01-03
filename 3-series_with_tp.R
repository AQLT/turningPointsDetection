library(AQLThesis)
library(future)
plan(multisession)
f <- "results/lp/UEMPLT5_parabolic_daf_tp.RDS"
data <- readRDS(f)
data$`1999.5`
c(AQLThesis::nber_tp_m)
compute_time_lag(data)

all_files <- list.files("results/lp/",pattern = "_tp",full.names = TRUE)
list_series <- list.files("data/byseries", full.names = TRUE)
names_series <- gsub(".RDS$", "", basename(list_series))
nom_f_s_tp <- sprintf("results/lp/%s_%s_%s_tp.RDS",
                      names_series,
          tolower("henderson"), tolower("daf"))
all_tp_fs <- lapply(nom_f_s_tp, function(f){
  future(compute_time_lag(readRDS(f)))
})
all_tp <- lapply(all_tp_fs, value)
all_t <- sapply(all_tp,`[[`,"troughs")
all_p <- sapply(all_tp,`[[`,"peaks")
colnames(all_t) <- colnames(all_p) <- gsub(".RDS$", "", basename(list_series))
series_with_tp <- names_series[!(apply(all_t, 2, \(x) all(is.na(x))) &
  apply(all_p, 2, \(x) all(is.na(x))))]
saveRDS(series_with_tp,"results/series_to_study.RDS")



library(tictoc)
tic()
all_tp_fs <- list()
for(i in seq_along(all_files)[1:10]){
  print(i)
  f = all_files[i]
  all_tp_fs[[i]] <- future(compute_time_lag(readRDS(f)))
}
all_tp <- lapply(all_tp_fs, value)
toc()

tic()

toc()
upt
data <- readRDS(f)
compute_time_lag(data)

compute_time_lag_future <- function(data,
         peaks = nber_tp_m[,"Peak"],
         troughs = nber_tp_m[,"Trough"],
         frequency = 12){
  peaks <- na.omit(peaks)
  troughs <- na.omit(troughs)
  
  peaks_timelag <- sapply(lapply(data, function(x){
    future(round(peaks,3) %in% round(do.call(c, x), 3))
  }), value)
  rownames(peaks_timelag) <- peaks
  troughs_timelag <- sapply(lapply(data, function(x){
    future(round(troughs,3) %in% round(do.call(c, x), 3))
  })
  rownames(troughs_timelag) <- troughs
  peaks_timelag <- sapply(rownames(peaks_timelag), function(tp){
    if(!any(peaks_timelag[tp,]) | peaks_timelag[tp,1])
      return(NA)
    first_est_tp <- colnames(peaks_timelag)[peaks_timelag[tp,]][1]
    (as.numeric(first_est_tp) - as.numeric(tp))*frequency
  })
  troughs_timelag <- sapply(rownames(troughs_timelag), function(tp){
    if(!any(troughs_timelag[tp,]) | troughs_timelag[tp,1])
      return(NA)
    first_est_tp <- colnames(troughs_timelag)[troughs_timelag[tp,]][1]
    (as.numeric(first_est_tp) - as.numeric(tp))*frequency
  })
  list(peaks = peaks_timelag,
       troughs = troughs_timelag)
}

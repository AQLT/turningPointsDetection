library(AQLThesis)
library(future)
plan(multisession)

all_files <- list.files("results/lp/",pattern = "_tp",full.names = TRUE)
list_series <- list.files("data/byseries", full.names = TRUE)
names_series <- gsub(".RDS$", "", basename(list_series))
nom_f_s_tp <- sprintf("results/lp/%s_%s_%s_tp.RDS",
                      names_series,
          tolower("henderson"), tolower("daf"))
all_tp <- lapply(nom_f_s_tp, function(f){
  compute_time_lag(readRDS(f))
})
all_t <- sapply(all_tp,`[[`,"troughs")
all_p <- sapply(all_tp,`[[`,"peaks")
colnames(all_t) <- colnames(all_p) <- gsub(".RDS$", "", basename(list_series))
series_with_tp <- names_series[!(apply(all_t, 2, \(x) all(is.na(x))) &
  apply(all_p, 2, \(x) all(is.na(x))))]
saveRDS(series_with_tp,"results/series_to_study.RDS")


series_with_tp = readRDS("results/series_to_study.RDS")
res = c()
for(f in list.files("data/byseriesinfo",full.names = TRUE)){
  info <- readRDS(f)
  if(length(unique(sapply(info,`[`,1)))>1){
    res = c(res, gsub(".RDS","", basename(f)))
  }
}
res = res[res %in% series_with_tp]
multiple_length <- sprintf("data/byseriesinfo/%s.RDS", res)
ml_data= sapply(multiple_length, function(f){
  info <- readRDS(f)
  sapply(info,`[`,1)
})
lapply(ml_data, table)
ml_data$`data/byseriesinfo/CES0600000007.RDS`

saveRDS(series_with_tp[!series_with_tp %in%res],
        "results/series_to_study_unique_length.RDS")

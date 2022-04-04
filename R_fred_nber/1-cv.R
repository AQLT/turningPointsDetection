library(AQLThesis)
library("future")
plan(multisession)
if(!dir.exists("data/byseriescv_nber"))
  dir.create("data/byseriescv_nber")

for(s in list.files("data/byseries_nber",full.names = TRUE)){
  print(s)
  data <- readRDS(s)
  info_fs <- lapply(data, function(x){
    future({sapply(3:12, function(i){
      tmp = CV(x, i)^2
      mean(tmp[-c(1:12, 1:12 + length(x) - 12)], na.rm=TRUE)})
    })
  })
  info <- sapply(info_fs, value)
  rownames(info) <- sprintf("h=%i", 3:12)
  saveRDS(info, sprintf("data/byseriescv_nber/%s", basename(s)))
}

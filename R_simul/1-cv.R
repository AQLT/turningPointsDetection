library(AQLThesis)
library("future")
plan(multisession)
if(!dir.exists("data_simul/byseriescv"))
  dir.create("data_simul/byseriescv")

for(s in list.files("data_simul/byseries",full.names = TRUE)){
  print(s)
  new_f = sprintf("data_simul/byseriescv/%s", basename(s))
  if(!file.exists(new_f)){
    
    data <- readRDS(s)
    info_fs <- lapply(data, function(x){
      future({sapply(3:12, function(i){
        tmp = CV(x, i)^2
        mean(tmp[-c(1:12, 1:12 + length(x) - 12)], na.rm=TRUE)})
      })
    })
    info <- sapply(info_fs, value)
    rownames(info) <- sprintf("h=%i", 3:12)
    saveRDS(info, new_f)
  }
}

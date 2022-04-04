library(AQLThesis)
library("future")
plan(multisession)
if(!dir.exists("data/byseriescv_nber"))
  dir.create("data/byseriescv_nber")

h_cv <- 18
for(s in list.files("data/byseries_nber",full.names = TRUE)){
  print(s)
  data <- readRDS(s)
  
  info_fs <- lapply(data, function(x){
    future({sapply(3:h_cv, function(i){
      tmp = CV(x, i)^2
      if(all(is.na(tmp)))
        return(NA)
      mean(tmp[-c(1:h_cv, 1:h_cv + length(x) - h_cv)], na.rm=TRUE)
    }
    )
    })
  })
  info <- sapply(info_fs, value)
  rownames(info) <- sprintf("h=%i", 3:h_cv)
  saveRDS(info, sprintf("data/byseriescv_nber/%s", basename(s)))
}

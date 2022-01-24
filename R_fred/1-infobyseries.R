library("future")
library(AQLThesis)
plan(multisession)

if(!dir.exists("data/byseriesinfo"))
  dir.create("data/byseriesinfo")

fs <- list()
i <- 0
for(s in list.files("data/byseries",full.names = TRUE)){
  i <- i+1
  print(s)
  fs[[i]] <- future({
    data <- readRDS(s)
    info <- lapply(data, function(x){
      res = AQLThesis::selectFilter(x)
      res = c(res[c("length", "icr")],
        AQLThesis::selectFilter(x, length = 9)[1], 
        AQLThesis::selectFilter(x, length = 23)[1])[c(1,3,2,4)]
      names(res) = c("optimal_length", "icr-9", "icr-13", "icr-23")
      res
      })
    saveRDS(info, sprintf("data/byseriesinfo/%s", basename(s)))
    s
  })
}
vs <- lapply(fs, value)

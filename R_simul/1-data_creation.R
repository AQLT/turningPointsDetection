if(!dir.exists("data_simul"))
  dir.create("data_simul")
if(!dir.exists("data_simul/byseries"))
  dir.create("data_simul/byseries")
if(!dir.exists("data_simul/byseriesinfo"))
  dir.create("data_simul/byseriesinfo")
library(AQLThesis)
set.seed(100)
start = 1948
frequency = 12
time = seq_along(seq(start, 2000+11/12, by = 1/12))
series = list(
  highvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 60,rho = 0.5),
  highvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 60,rho = 0.7),
  highvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 60,rho = 0.8),
  lowvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.20,lambda = 60,rho = 3),
  lowvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.20,lambda = 60,rho = 3.5),
  lowvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.20,lambda = 60,rho = 4)
              )
series = lapply(series,ts, start = start, frequency = frequency)

tp = turning_points(series[[6]][,"cycle"])
first_date = time(series[[6]][,"cycle"])[25]
tp = lapply(tp, function(x)x[x>=first_date])
saveRDS(tp, "data_simul/tp_simul1.RDS")

for(nom_series in names(series)){
  all_series <- lapply(time[-(1:24)], function(i){
    ts(series[[nom_series]][1:i,"tc"], start = start, frequency = frequency)
  })
  names(all_series) <- sapply(all_series, function(x) time(x)[length(x)])
  saveRDS(all_series, file = sprintf("data_simul/byseries/%s.RDS", nom_series))
}


library("future")
plan(multisession)


fs <- list()
i <- 0
for(s in list.files("data_simul/byseries",full.names = TRUE)){
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
    saveRDS(info, sprintf("data_simul/byseriesinfo/%s", basename(s)))
    s
  })
}
vs <- lapply(fs, value)

do.call(rbind,readRDS(sprintf("data_simul/byseriesinfo/%s.RDS", "highvariability1")))
tail(do.call(rbind,readRDS(sprintf("data_simul/byseriesinfo/%s.RDS", "lowvariability3"))))


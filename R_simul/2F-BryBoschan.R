if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/bb"))
  dir.create("results_simul/bb")
library(Coinprofile)
library(AQLThesis)
library("future")
plan(multisession)
bb_tp = function(x){
  first_data = start(x)
  tp = TP_BryBoschan(x, year = first_data[1],
                     month = first_data[2],frequ = frequency(x))
  date_tp = time(x)[tp$Time]
  list(upturn = date_tp[tp$Max_Min == 1],
       downturn = date_tp[tp$Max_Min == 2])
}

list_series <- list.files("data_simul/byseries", full.names = TRUE)
s = list_series[1]
fs <- list()
j <- 1
reload <- FALSE

for(s in list_series){
  name_file <- gsub(".RDS$", "", basename(s))
  print(name_file)
  data <- readRDS(s)
  data_info <- readRDS(sub("byseries", "byseriesinfo", s))
  nom_f_s <- sprintf("results_simul/bb/%s.RDS",
                     name_file)
  nom_f_s_tp <- 
    sprintf("results_simul/bb/%s_tp.RDS",
            name_file)
  
  if(all(file.exists(nom_f_s_tp)))
    next;
  
  reload <- TRUE
  fs[[j]] <- future({
    print(s)
    tp <- lapply(names(data), function(nom_d){
      x <- data[[nom_d]]
      bb_tp(x)
    })
    names(tp) <- names(data)
    
    saveRDS(tp,
            nom_f_s_tp)
    TRUE
  })
  j <- j+1
}
if(reload){
  vs <- lapply(fs, value)
  fs <- list()
  j <- 1
}


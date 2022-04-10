if(!dir.exists("results_nber"))
  dir.create("results_nber")
if(!dir.exists("results_nber/bb"))
  dir.create("results_nber/bb")
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

list_series <- list.files("data/byseries_nber", full.names = TRUE)

fs <- list()
i <- 0

for(s in list_series){
  name_file <- gsub(".RDS$", "", basename(s))
  print(name_file)
  
  nom_f_s_tp <- 
    sprintf("results_nber/bb/%stp.RDS",
            name_file)
  
  if(all(file.exists(nom_f_s_tp)))
    next;
  
  i <- i+1
  
  fs[[i]] <- future({
    print(s)
    data <- readRDS(s)
    data_info <- readRDS(sub("byseries_nber", "byseriesinfo_nber", s))
    
    tp <- lapply(names(data), function(nom_d){
      x <- data[[nom_d]]
      bb_tp(x)
    })
    names(tp) <- names(data)
    
    saveRDS(tp,
            nom_f_s_tp)
    TRUE
  })
}


vs <- lapply(fs, value)

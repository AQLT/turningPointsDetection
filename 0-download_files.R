if(!dir.exists("data"))
  dir.create("data")
if(!dir.exists("data/csv"))
  dir.create("data/csv")
if(!dir.exists("data/rds"))
  dir.create("data/rds")

overwrite <- TRUE

# Téléchargement des bases FRED-MD
download.file("https://s3.amazonaws.com/files.research.stlouisfed.org/fred-md/Historical_FRED-MD.zip",
              "data/Historical_FRED-MD.zip")
download.file("https://s3.amazonaws.com/files.research.stlouisfed.org/fred-md/FRED_MD.zip",
              "data/FRED_MD.zip")

list_hist_files <- unzip("data/Historical_FRED-MD.zip",exdir = "data", overwrite = overwrite)
list_recent_files <- unzip("data/FRED_MD.zip",exdir = "data", overwrite = overwrite)

# On deplace tous les fichiers .csv sous data/csv
filesstrings::move_files(grep("\\.csv$", list_hist_files, value = TRUE), "data/csv/", overwrite = overwrite)
filesstrings::move_files(grep("\\.csv$", list_recent_files, value = TRUE), "data/csv/",overwrite = overwrite)

# on deplace tous les autres fichiers sous data
filesstrings::move_files(grep("\\.csv$", list_hist_files, invert = TRUE, value = TRUE), "data/", overwrite = overwrite)
filesstrings::move_files(grep("\\.csv$", list_recent_files, invert = TRUE, value = TRUE), "data/",overwrite = overwrite)

# On supprime les dossiers inutiles
unlink(unique(dirname(list_hist_files)), recursive = TRUE)
unlink(unique(dirname(list_recent_files)), recursive = TRUE)

# Finalement pas top car fonction non à jour :
# s'appuie sur une vieille specif (mauvaises transformations, mauvais noms colonnes, etc.), dernière date non importée, etc.
# if(!require(fbi)){
#   devtools::install_github("cykbennie/fbi")
#   library(fbi)
# }
# data <- fbi::fredmd("data/2021-10.csv")

library(tictoc)
tic()
for(f in list.files(path = "data/csv", pattern = "\\.csv$",full.names = TRUE)){
  print(f)
  data <- AQLThesis::fredmd(file = f)
  saveRDS(data, sub("csv/", "rds/",sub("\\.csv$",".RDS", f), fixed = TRUE))
}
toc()

# version plus rapide :
library("future")
plan(multisession)

fs <- list()
i <- 0
for(f in list.files(path = "data/csv", pattern = "\\.csv$",full.names = TRUE)){
  i <- i+1
  
  print(f)
  fs[[i]] <- future({
    data <- AQLThesis::fredmd(file = f)
    saveRDS(data, sub("csv/", "rds/",sub("\\.csv$",".RDS", f), fixed = TRUE))
  })
}
vs <- lapply(fs, value)

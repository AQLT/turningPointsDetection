if(!dir.exists("data"))
  dir.create("data")
if(!dir.exists("data/csv"))
  dir.create("data/csv")
if(!dir.exists("data/rds"))
  dir.create("data/rds")

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
file.remove("data/csv/current.csv")

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
  
  fs[[i]] <- future({
    print(f)
    data <- AQLThesis::fredmd(file = f, transform = FALSE, log = TRUE)
    saveRDS(data, sub("csv/", "rds/",sub("\\.csv$",".RDS", f), fixed = TRUE))
  })
}
vs <- lapply(fs, value)

## creation data par serie et par date

if(!dir.exists("data/byseries"))
  dir.create("data/byseries")
series <- unique(unlist(lapply(list.files("data/rds",full.names = TRUE),
                               function(x)colnames(readRDS(x)))))

library("future")
plan(multisession)

fs <- list()
i <- 0
for(s in series){
  i <- i+1
  fs[[i]] <- future({
    print(s)
    full_data <- lapply(list.files("data/rds",full.names = TRUE),
                        function(date){
                          tmp = readRDS(date)
                          if(! s%in% colnames(tmp))
                            return(NULL)
                          #remove some NA
                          na.omit(na.contiguous(tmp[,s]))
                        })
    last_dates <- sapply(full_data, function(x){
      if(is.null(x))
        return(NA)
      tail(time(x),1)
      })
    names(full_data) <- last_dates
    full_data <- full_data[!is.na(last_dates)]
    # remove duplicated dates (no extra data in next fred database)
    full_data <- full_data[!duplicated(names(full_data))]
    
    #at least 23 data
    full_data <- full_data[sapply(full_data, function(x) length(x)>=23)]
    saveRDS(full_data, sprintf("data/byseries/%s.RDS", s))
    s
  })
}
vs <- lapply(fs, value)


res = lapply(list.files("data/byseries",full.names = TRUE), function(s){
  data <- readRDS(s)
  l_  = sapply(data, length)
  diff(l_)
})
names(res) <- gsub(".RDS", "",
                   basename(list.files("data/byseries",full.names = TRUE)))
res2 <- sapply(res,function(x){
  criteria = x>2
  if(!any(criteria))
    return(NULL)
  x[criteria]
})
res2 <- res2[!sapply(res2, is.null)]
res2
names(res2)
length(res)
sapply(res, function(x) x[which(x <=-1)])

head(res[1])
names(res)
res[[1]][which(res[[1]] <= -1)]
list.files("data/byseries",full.names = TRUE)[3]



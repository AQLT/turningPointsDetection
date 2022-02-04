if(!dir.exists("data/byseries_nber"))
  dir.create("data/byseries_nber")
series_to_keep <- c(
  "W875RX1", # Real personal income ex transfer receipts
  "INDPRO", #industrial production
  "PAYEMS", # All Employees: Total nonfarm
  "DPCERA3M086SBEA", #Real personal consumption expenditures 
  "RETAILx", #Retail sales
  "CE16OV" #employment as measured by the household survey
)

files =list.files("data/byseries",
           pattern = paste(series_to_keep, collapse = "|"),
           full.names = TRUE
           )
file.copy(files,
          gsub("byseries",
               "byseries_nber",
               files))

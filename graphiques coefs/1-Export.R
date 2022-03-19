source(file = "graphiques coefs/0-fonctions.R",encoding = "UTF-8")

rkhs = readRDS("filters/rkhs_rw_p3.RDS")[["h=6"]]
lp <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
  lp_filter(
    horizon = 6,
    degree = 3,
    kernel = "Henderson",
    endpoints = endpoints,
    ic = 3.5,
    tweight = 0,
    passband = pi/12
  )$filters.coef
})
all_filters <- c(lapply(rkhs, function(x){
  colnames(x) <- colnames(lp[[1]])
  x
}),
lp)

names(all_filters) <- c("frf", "gain", "phase",
                        "LC", "QL", "CQ", "DAF")

if(!dir.exists("graphiques coefs/filters_used")){
  dir.create("graphiques coefs/filters_used")
}
for(i in names(all_filters)){
  print(i)
  p <- plot_graph(all_filters[[i]])
  ggsave(filename = sprintf("graphiques coefs/filters_used/%s.pdf",tolower(i)), 
         p,
         width = 8, height = 5)
}
for(i in names(all_filters)){
  print(i)
  p <- plot_graph(all_filters[[i]])
  ggsave(filename = sprintf("graphiques coefs/filters_used/%s.svg",tolower(i)), 
         p,
         width = 8, height = 5)
}

for(i in names(all_filters)){
  print(i)
  p <- plot_graph(all_filters[[i]])
  ggsave(filename = sprintf("graphiques coefs/filters_used/%s.jpg",tolower(i)), 
         p,
         width = 8, height = 5)
}

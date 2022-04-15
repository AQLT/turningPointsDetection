library(dplyr)
library(ggplot2)
library(plot3D)
library(magrittr)

series_with_tp <- readRDS("results/series_to_study.RDS")
length_info <- 
  sapply(sprintf("data/byseriesinfo/%s.RDS", series_with_tp), 
         function(f){
           info <- readRDS(f)
           info[[1]][1]
         })
length_info <- data.frame(series = series_with_tp,
                          length = as.numeric(length_info))

troughs_detected <- t(sapply(list.files("results/compile_tp",pattern = "troughs",full.names = TRUE), function(f){
  all_t <- readRDS(f)
  !apply(all_t[,1:35],2,\(x) all(is.na(x)))
}))

peaks_detected <- t(sapply(list.files("results/compile_tp",pattern = "peaks",full.names = TRUE), function(f){
  all_t <- readRDS(f)
  !apply(all_t[,1:34],2,\(x) all(is.na(x)))
}))



# dput(c(colnames(troughs_detected)[apply(troughs_detected,2, any)],
#        colnames(peaks_detected)[apply(peaks_detected,2, any)]))
# 
# dput(c(colnames(troughs_detected)[apply(!troughs_detected,2, all)],
#        colnames(peaks_detected)[apply(!peaks_detected,2, all)]))
selected_tp <- c("X2001.83333333333", "X2009.41666666667", "X2020.25", "X2001.16666666667", 
                 "X2007.91666666667", "X2020.08333333333")
not_selected_tp <- c("X1854.91666666667", "X1858.91666666667", "X1861.41666666667", 
                     "X1867.91666666667", "X1870.91666666667", "X1879.16666666667", 
                     "X1885.33333333333", "X1888.25", "X1891.33333333333", "X1894.41666666667", 
                     "X1897.41666666667", "X1900.91666666667", "X1904.58333333333", 
                     "X1908.41666666667", "X1912", "X1914.91666666667", "X1919.16666666667", 
                     "X1921.5", "X1924.5", "X1927.83333333333", "X1933.16666666667", 
                     "X1938.41666666667", "X1945.75", "X1949.75", "X1954.33333333333", 
                     "X1958.25", "X1961.08333333333", "X1970.83333333333", "X1975.16666666667", 
                     "X1980.5", "X1982.83333333333", "X1991.16666666667", "X1857.41666666667", 
                     "X1860.75", "X1865.25", "X1869.41666666667", "X1873.75", "X1882.16666666667", 
                     "X1887.16666666667", "X1890.5", "X1893", "X1895.91666666667", 
                     "X1899.41666666667", "X1902.66666666667", "X1907.33333333333", 
                     "X1910", "X1913", "X1918.58333333333", "X1920", "X1923.33333333333", 
                     "X1926.75", "X1929.58333333333", "X1937.33333333333", "X1945.08333333333", 
                     "X1948.83333333333", "X1953.5", "X1957.58333333333", "X1960.25", 
                     "X1969.91666666667", "X1973.83333333333", "X1980", "X1981.5", 
                     "X1990.5")
not_selected_tp_grep <- paste(sprintf("(%s)", not_selected_tp), collapse = "|")
select_var <- function(x){
  # x = x[x$series %in% series_with_tp_ul,
  #       grep(not_selected_tp_grep, colnames(x), invert = TRUE)]
  # x = merge(x, length_info, all.x = TRUE, all.y = FALSE)
  x = select_series(x)
  x = x[,
        grep(not_selected_tp_grep, colnames(x), invert = TRUE)]
  toNA <- apply(x[,grep("^X", colnames(x))],2, function(x_){
    res = x_ > x$length
    res[is.na(res)] <- FALSE
    res
  } )
  x[,grep("^X", colnames(x))][toNA] <- NA
  x
}
select_series <- function(x){
  # x = x[x$series %in% series_with_tp_ul,]
  x = merge(x, length_info, all.x = TRUE, all.y = FALSE)
  x
}
nb_obs <- function(x){
  sum(!is.na(x))
}

select_mae <- function(x){
  x %>% 
    filter(Group == "total",
           stats == "MAE") %>% 
    select(!c(Group, stats))
}

plot_fst_tp <- function(plot_data, degree = 0, length = 13, color = "moy",
                        limits = TRUE, title = NULL){
  
  if(missing(title))
    title = sprintf("color variable = %s, degree = %i, length = %i",
                    color, degree, length)
  plot_data <- plot_data[plot_data$length %in% length,]
  limits_ = range(plot_data[,color])
  
  plot_data <- plot_data[plot_data$degree %in% degree,
                         # c("fidelity.weight","smoothness.weight", "timeliness.weight",color)
  ]
  
  if(limits){
    plot_data <- rbind(plot_data, 
                       c(-1, -1, -1, limits_[1]),
                       c(-1, -1, -1, limits_[2]))
  }
  with(plot_data, 
       scatter3D(x = fidelity.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color], 
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 1,
                 # cex = 0.1, alpha = 0.4,
                 ticktype = "detailed",
                 xlim = c(0,1),
                 ylim = c(0,1),
                 zlim = c(0,1), 
                 xlab = "\n\nFidelity",
                 ylab ="\n\nSmoothness",
                 zlab = "\n\nTimeliness",
                 main = title))
}

plot_fst_rev <- function(plot_data, degree = 0, length = 13, color = "rev.q0",
                         limits = TRUE, group = "total", stats = "RMSE",
                         title = NULL){
  if(missing(title))
    title = sprintf("color variable = %s of %s, degree = %i, length = %i",
                    stats,
                    color, degree, length)
  plot_data <- plot_data[(plot_data$length %in% length) &
                           (plot_data$Group == group) &
                           (plot_data$stats == stats),]
  limits_ = range(plot_data[,color])
  
  plot_data <- plot_data[plot_data$degree %in% degree,
                         # c("fidelity.weight","smoothness.weight", "timeliness.weight",color)
  ]
  
  if(limits){
    plot_data <- rbind(plot_data, 
                       c(-1, -1, -1, limits_[1]),
                       c(-1, -1, -1, limits_[2]))
  }
  with(plot_data, 
       scatter3D(x = fidelity.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color], 
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 1,
                 # cex = 0.1, alpha = 0.4,
                 ticktype = "detailed",
                 xlim = c(0,1),
                 ylim = c(0,1),
                 zlim = c(0,1), 
                 xlab = "\n\nFidelity",
                 ylab ="\n\nSmoothness",
                 zlab = "\n\nTimeliness",
                 main = title))
}

plot_est <- function (data, date_tp =2020.25, titre = NULL, sous_titre = NULL, limits_y = NULL) {
  cex = 0.4;
  x_lab = y_lab= NULL; x_lab_month = TRUE; 
  outDec = ",";  n_xlabel = 6 ;n_ylabel = 4; 
  size = 0.7; size_label = 2
  
  dataGraph <- format_data_plot(data)
  data_legend = dataGraph %>% 
    group_by(variable) %>%
    filter(date == max(date)) %>% data.frame()
  p <- ggplot(data = dataGraph) +
    geom_vline(xintercept = date_tp, linetype = "dotted") +
    geom_line(mapping = aes(x = date, y = value, group = variable, 
                            colour = variable), size = size) + 
    labs(title = titre, subtitle = sous_titre, x = x_lab, 
         y = y_lab) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel), 
                       labels = \(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month, 
                                                                 outDec = outDec)) + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel), 
                       labels = function(x) format(x, decimal.mark = outDec),
                       limits = limits_y) + 
    AQLTools:::theme_aqltools()
  p +
    geom_text(aes(x = date, y = value, label =variable, colour = variable), data = data_legend,
               check_overlap = TRUE, hjust = 0, nudge_x = 0.01,
               size = size_label) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size=8, angle=20,
                                     vjust=1.1, hjust=1),
          axis.text.y = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,face="italic"))
}
format_data_plot  <- function(data){
  time <- time(data)
  freq <- frequency(data)
  dataGraph <- data.frame(cbind(time, data))
  colnames(dataGraph) <- c("date", colnames(data))
  reshape2::melt(dataGraph, id = "date") |> na.omit()
}
plot_prevs <- function (data, data_prevs, date_tp =2020.25, titre = NULL, sous_titre = NULL, limits_y = NULL,
                        linetype_prev = "dashed") {
  cex = 0.4;
  x_lab = y_lab= NULL; x_lab_month = TRUE; 
  outDec = ",";  n_xlabel = 6 ;n_ylabel = 4; 
  size = 0.7; size_label = 2
  
  
  dataGraph <- format_data_plot(data)
  dataGraph_prevs <- format_data_plot(data_prevs)
  data_legend = dataGraph_prevs  %>% 
    group_by(variable) %>%
    filter(date == max(date)) %>% data.frame()
  p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable, 
                                    colour = variable)) +
    geom_vline(xintercept = date_tp, linetype = "dotted") +
    geom_line(size = size) + 
    geom_line(data = dataGraph_prevs, aes(x = date, y = value, group = variable, 
                                          colour = variable), size = 0.6, linetype = linetype_prev)+
    labs(title = titre, subtitle = sous_titre, x = x_lab, 
         y = y_lab) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel), 
                       labels = \(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month, 
                                                                 outDec = outDec)) + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel), 
                       labels = function(x) format(x, decimal.mark = outDec),
                       limits = limits_y) + 
    AQLTools:::theme_aqltools()
  p +
    geom_text(aes(x = date, y = value, label =variable, colour = variable), data = data_legend,
              check_overlap = TRUE, vjust = 1, nudge_y = -0.01,
              size = size_label) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size=8, angle=20,
                                     vjust=1.1, hjust=1),
          axis.text.y = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,face="italic"))
}
extract_est_data <- function(method = "lc", kernel = "henderson", nb_est = 10,
                             series = "RETAILx",
                             tp_date = 2020.25,
                             nb_dates_before = 6){
  # print(method)
  sep = "_"
  if(method %in% c("lc","ql", "cq", "daf")){
    dir <- "lp"
    full_name <- sprintf("%s_%s", kernel, method)
  }else{
    if(length(grep("arima", method)) >0){
      dir <- "arima"
      full_name <- sep <- ""
    }else if (length(grep("_h.", method)) >0){
      dir <- "localic_lp"
      if (length(grep("ql", method)) > 0) {
        method <- gsub("_d3", "", method)
      }
      full_name <- method
      
    } else {
      dir <- "rkhs"
      full_name <- method
    }
  }
  file = sprintf("results_nber/%s/%s%s%s.RDS", dir, series, sep, full_name)
  data <- readRDS(file)
  data <- do.call(ts.union, data)
  colnames(data) <- as.character(zoo::as.yearmon(as.numeric(colnames(data))))
  column_to_keep <- !apply(window(data, start = tp_date),2, \(x) all(is.na(x)))
  data <- data[,column_to_keep]
  data <- data[,1:nb_est]
  last_date_est = zoo::na.trim(data[,ncol(data)], sides = "left")
  last_date <- time(last_date_est)[which(is.na(last_date_est))[1] - 1]
  window(data, start = tp_date - nb_dates_before/frequency(data),
         end = last_date)
}
extract_est_data_fst <- function(weight = "weight231",
                                 degree = 0,
                                 nb_est = 10,
                             series = "RETAILx",
                             tp_date = 2020.25,
                             nb_dates_before = 6){

  file = sprintf("results_nber/fst/fst%s/%s_degree%s_%s.RDS",degree, series, degree, weight)
  data <- readRDS(file)
  data <- do.call(ts.union, data)
  colnames(data) <- as.character(zoo::as.yearmon(as.numeric(colnames(data))))
  column_to_keep <- !apply(window(data, start = tp_date),2, \(x) all(is.na(x)))
  data <- data[,column_to_keep]
  data <- data[,1:nb_est]
  last_date <- time(data)[which(is.na(zoo::na.trim(data[,ncol(data)], sides = "left")))[1] - 1]
  window(data, start = tp_date - nb_dates_before/frequency(data),
         end = last_date)
}


format_fst <- function(x){
  weights_ordered = sprintf("weight%i", sort(unique(as.numeric(gsub("weight", "", x$weight)))))
  x <- x |> 
    mutate(weight = factor(weight, 
                           levels = weights_ordered,
                           ordered = TRUE))
  if("variability" %in% colnames(x)){
    x <- x |> 
      mutate(variability = factor(variability,
                                  levels = c("lowvariability","mediumvariability","highvariability"),
                                  ordered = TRUE))
  }
  x
}

format_ref_fst <- function(x){
  x |> 
    filter(method == "lc", kernel == "henderson")  %>% 
    set_rownames(.$series) |> 
    select(!c(method, kernel, length, series))
  
}
normalise_fst <- function(x, suff = "^(rev|X)"){
  name_x <- gsub("_fst$", "", deparse(substitute(x)))
  ref = get(paste0(name_x,"_ref"))
  for(w in unique(x$weight)){
    for(d in 0:3){
      indices = x$weight==w & x$degree ==d
      x[indices,grep(suff,colnames(x))] <- 
        x[indices,grep(suff,colnames(x))] / ref[x[indices,"series"],]
    }
  }
  x
}

ggMultisave <- function(filename, out = c("pdf","jpg","svg"),...){
  invisible(lapply(sprintf("%s.%s", gsub("\\.$","",filename), out),
                   ggsave,...))
}
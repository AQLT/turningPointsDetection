library(dplyr)

series_with_tp_ul <- readRDS("results/series_to_study_unique_length.RDS")
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
  x = x[x$series %in% series_with_tp_ul,]
  x = merge(x, length_info, all.x = TRUE, all.y = FALSE)
  x
}
nb_obs <- function(x){
  sum(!is.na(x))
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
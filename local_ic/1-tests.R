library(rjd3filters)
h <- 6 
q <- 6
X_gen <- function(d = 1, p = 6, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}
gen_MM <- function(p=6, q=p, d=2){
  k = rjd3filters::get_kernel("Henderson", h = h)
  k
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  k
  K = diag(k)
  X = X_gen(d=d, p = h, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  cbind(M1, M2, M3)
}
MM2 = sapply(0:6, function(x) c(gen_MM(d=3,q=x)[,2], rep(0, h-x)))
round(gen_MM(d=2) - gen_MM(d=3),3)
plot_coef(moving_average(gen_MM(d=3)[,2], -6))
plot_coef(moving_average(gen_MM(d=2)[,2], -6),add = TRUE)
plot_coef(moving_average(gen_MM(d=3,q=4)[,2], -6),add = TRUE)

h_filter = lp_filter(horizon = h,endpoints = "DAF")$filters.coef
henderson_coef = h_filter[,ncol(h_filter)]

s = "data_simul/byseries/mediumvariability2.RDS"
new_f = sprintf("data_simul/byseriescv/%s", basename(s))

tp = readRDS("data_simul/tp_simul1.RDS")
data <- readRDS(s)
data_info <- readRDS(sub("byseries", "byseriesinfo", s))
sigma_2 = sapply(data, function(x){
  mean((x - henderson(x,length = 2*h+1,musgrave = F))^2,na.rm = TRUE)
})
sigma_2 = sigma_2/(1- 2*henderson_coef["t"] + sum(henderson_coef^2))
icr = ts(sapply(data_info,`[`,"icr-13"), start = 1962, frequency = 12)
rapport_act = 2/(sqrt(pi) * icr)
rapport_act = rapport_act^2
x = data[[length(data)]]
estim_deriv = lapply(0:h, function(q) jasym_filter(x, MM2[,q+1], -6))
estim_local_rapport = lapply(estim_deriv, function(x) x^2 / ts(sigma_2, start= start(na.omit(x)), frequency = 12))

lc_tp = readRDS("results_simul/lp/mediumvariability2_henderson_lc_tp.RDS")

tp = lc_tp[[length(lc_tp)]]

for(i in tp$upturn){
  abline(v=i,col = "red")
}
for(i in tp$downturn){
  abline(v=i,col = "blue")
}

estim_deriv_abs = lapply(estim_deriv, abs)
highchart(type = "stock") |> 
  hc_add_series(data = estim_deriv_abs[[7]],
                name = "Estimation finale") |> 
  hc_add_series(data = lag(estim_deriv_abs[[7]],-6),
                name = "Estimation à partir de h-6") |> 
  hc_add_series(data = lag(estim_deriv_abs[[6]],-5),
                name = "Estimation à partir de h-5") |> 
  hc_add_series(data = lag(estim_deriv_abs[[5]],-4),
                name = "Estimation à partir de h-4") |> 
  hc_add_series(data = lag(estim_deriv_abs[[4]],-3),
                name = "Estimation à partir de h-3")|> 
  hc_add_series(data = lag(estim_deriv_abs[[3]],-2),
                name = "Estimation à partir de h-2")|> 
  hc_add_series(data = lag(estim_deriv_abs[[2]],-1),
                name = "Estimation à partir de h-1")|> 
  hc_add_series(data = lag(estim_deriv_abs[[1]],0),
                name = "Estimation à partir de h") |> 
  hc_legend(enabled = TRUE)

sapply(lapply(0:6, function(q) estim_deriv_abs[[7]]-lag(estim_deriv_abs[[q+1]],-q)),
       function(x) mean(x^2, na.rm = TRUE))

library(highcharter)
hc <- highchart(type = "stock") |> 
  hc_add_series(data = estim_local_rapport[[7]],
                name = "Estimation local du paramètre (delta/sigma)^2") |> 
  hc_add_series(data = rapport_act,
                name = "Estimation à partir du ratio I/C")|>
  hc_legend(enabled = TRUE) %>% 
  hc_tooltip(pointFormat = sprintf("{series.name}: <b>{point.y:.%if}</b><br/>", 
                                   2))
hc

color = c(lapply(sprintf("Date.UTC(%s,%s,01)", format(zoo::as.yearmon(tp$upturn), "%Y"),
                         format(zoo::as.yearmon(tp$upturn), "%m")),
                 function(x) 
                   list(color = "#FF0000", width = 1,
                        value = JS(x))),
          lapply(sprintf("Date.UTC(%s,%s,01)", format(zoo::as.yearmon(tp$downturn), "%Y"),
                         format(zoo::as.yearmon(tp$downturn), "%m")),
                 function(x) 
                   list(color = "#90ee90", width = 1,
                        value = JS(x)))) |
hc |> 
  hc_xAxis(plotLines = color)  

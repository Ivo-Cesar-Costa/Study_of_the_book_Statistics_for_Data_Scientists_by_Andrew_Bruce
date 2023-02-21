# CAPITULO 2 - DISTRIBUIÇÕES DE DADOS E AMOSTRAS

## Distribuição de Amostragem de uma Estatística

library(ggplot2)
samp_data <- data.frame(income=sample(loans_income, 1000),
                        type="data_dist")
samp_mean_05 <- data.frame(income=tapply(sample(loans_income, 1000*5),
                                         rep(1:1000, rep(5,1000)), FUN = mean),
                           type="mean_of_5")
samp_mean_20 <- data.frame(income=tapply(sample(loans_income, 1000*20),
                                         rep(1:1000, rep(20,1000)), FUN = mean),
                           type="mean_of_20")
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type, 
                     levels = c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels = c('Data', 'mean_of_5', 'mean_of_20'))
ggplot(income, aes(x=income)) + 
  geom_histogram(bins = 40) + 
  facet_grid(type ~.)

## O Bootstrap

library(boot)
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R = 1000, statistic = stat_fun)

## Normal Padrão e Gráficos QQ

norm_samp <- rnorm(100)
qqnorm(norm_samp)
abline(a=0, b=1, col='grey')

## Distribuições de Cauda Longa

nflx <- sp500_px[,'NFLX']
nflx <- diff(log(nflx[nflx>0]))
qqnorm(nflx)
abline(a=0, b=1, col='grey')

## Distribuição Binomial

dbinom(x=2, n=5, p=0.1)
pbinom(2, 5, 0.1)

## Distribuição Poisson

rpois(100, lambda = 2)

## Distribuição Exponencial

rexp(n = 100, rate = .2)

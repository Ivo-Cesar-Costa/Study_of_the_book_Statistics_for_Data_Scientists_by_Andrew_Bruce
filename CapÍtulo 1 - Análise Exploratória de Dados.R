## CAPITULO 1 - ANÁLISE EXPLORATÓRIA DE DADOS

## Exemplo: Estimativas de Localização de População e Taxas de Homicídio

PSDS_PATH <- file.path('C:/Users/Ivo/Downloads/Genius/Estatistica_pratica_para_cientistas_de_dados', 'statistics-for-data-scientists')

state <- read.csv(file.path(PSDS_PATH, 'data', 'state.csv'), row.names = 1, sep = ";")
mean(state[["Population"]])
mean(state[["Population"]], trim = 0.1)
median(state[["Population"]])
weighted.mean(state[['Murder.Rate']], w = state[['Population']])
library(matrixStats)
weightedMedian(state[['Murder.Rate']], w = state[['Population']])

## Exemplo: Estimativas de Variabilidade de População Estadual

sd(state[['Population']])
IQR(state[['Population']])
mad(state[['Population']])

## Percentis e Boxplots

quantile(state[['Murder.Rate']], p = c(.05, .25, .5, .75, .95))

boxplot(state[['Population']]/1000000, ylab = "Population (millions)")

## Tabela de Frequências e Histogramas

breaks <- seq(from=min(state[['Population']]),
              to=max(state[['Population']]), length=11)
pop_freq <- cut(state[['Population']], breaks = breaks,
                right = TRUE, include.lowest = TRUE)
table(pop_freq)
hist(state[['Population']], breaks = breaks)

## Estimativas de Densidade

hist(state[['Murder.Rate']], freq = FALSE)
lines(density(state[['Murder.Rate']]), lwd = 3, col = "blue")

## Explorando Dados Binários e Categóricos

barplot(as.matrix(dfw)/6, cex.axis = .5)

## Correlação

sp500_px <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_px.csv'), sep = ";")
sp500_sym <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_sym.csv'), sep = ";")
etfs <- sp500_px[row.names(sp500_px)>"2012-07-01",
                 sp500_px[sp500_sym$sector=="etf",'symbol']]
library(corrplot)
corrplot(cor(etfs), method = "ellipse")

## Gráficos de Dispersão

plot(telecom$T,telecom$VZ, xlab = "T", ylab = "VZ")

## Compartimentação Hexagonal e Contornos (Representando Númericos versus Dados Númericos)

kc_tax <- read.csv(file.path(PSDS_PATH, 'data', 'kc_tax.csv'), sep = ";")
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving>100 & SqFtTotLiving<3500)
nrow(kc_tax0)

library(ggplot2)

ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
  stat_binhex(colour = "white") + 
  theme_bw() +
  #scale_fill_gradient(low = "white", high = "black") +
  labs(x = "Finished Square Feet", y = "Tax Assessed Value")

ggplot(kc_tax0, aes(x=SqFtTotLiving, y=TaxAssessedValue)) +
  theme_bw() +
  geom_point( alpha=0.1) + 
  geom_density2d(colour="white") + 
  labs(x = "Finished Square Feet", y = "Tax Assessed Value")

## Duas Variáveis Categóricas

library(descr)
x_tab <- CrossTable(lc_loans$grade, lc_loans$status,
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

## Dados Categóricos e Numéricos

boxplot(pct_delay ~ airline, data = airline_stats, ylim=c(0,50))

ggplot(data = airline_stats, aes(airline, pct_carrier_delay)) + 
  ylim(0,50) + 
  geom_violin() + 
  labs(x = "", y = "Daily % of Delayed Flights")

## Visualizando Variáveis Múltiplas

ggplot(subset(kc_tax0, ZipCode %in% c(98188,98105,98108,98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour="white") +
  theme_bw() + 
  #scale_fill_gradient( low="white", high="blue") + 
  labs(x = "Finished Square Feet", y = "Tax Assessed Value") + 
  facet_wrap("ZipCode")
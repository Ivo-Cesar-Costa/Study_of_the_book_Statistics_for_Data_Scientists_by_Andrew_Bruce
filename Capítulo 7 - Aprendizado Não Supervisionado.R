# CAPÍTULO 7 - APRENDIZADO NÃO SUPERVISIONADO

options(max.print=20)

## Um Exemplo Simples

oil_px <- sp500_px[,c('CVX','XOM')]
pca <- princomp(oil_px)
pca$loadings

library(ggplot2)

#loadings <- pca$loadings
#ggplot(data = oil_px, aes(x=CVX,y=XOM)) + 
#  geom_point(alpha=.3) + 
#  stat_ellipse(type='norm', level = .99) + 
#  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1]) + 
#  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2])

## Interpretando os Componentes Principais

library(dplyr)

syms <- c('AAPL','MSFT','CSCO','INTC','CVX','XOM','SLB','COP','JPM','WFC','USB','AXP',
          'WMT','TGT','HD','COST')
top_sp <- sp500_px[rownames(sp500_px)>='2005-01-01', syms]
sp_pca <- princomp(top_sp)
screeplot(sp_pca)

library(tidyr)
loadings <- sp_pca$loadings[,1:5]
loadings$Symbol <- row.names(loadings)
loadings <- gather(loadings,"Component", "Weight", -Symbol)
ggplot(loadings, aes(x=Symbol, y=Weight)) + 
  geom_bar(stat = 'identity') + 
  facet_grid(Component ~ .,scales = 'free_y')

## Um Exemplo Simples

df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM','CVX')]
km <- kmeans(df, centers = 4)

df$cluster <- factor(km$cluster)
head(df)

centers <- data.frame(cluster = factor(1:4), km$centers)
centers
ggplot(data =df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) + 
  geom_point(alpha=.3) + 
  geom_point(data=centers, aes(x=XOM, y=CVX), size=3, stroke=2)

## Algoritmo de K-Médias

syms <- c('AAPL','MSFT','CSCO','INTC','CVX','XOM','SLB','COP','JPM','WFC','USB','AXP',
          'WMT','TGT','HD','COST')
df <- sp500_px[rownames(sp500_px)>='2011-01-01', syms]
km <- kmeans(df, centers = 5, nstart=10)

## Interpretando os Agrupamentos

km$size

centers <- as.data.frame(t(centers))
names(centers) <- paste("Cluster",1:5)
centers$Symbol <- row.names(centers)
centers <- gather(centers,"Cluster","Mean",-Symbol)
centers$Color = centers$Mean > 0
ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) + 
  geom_bar(stat='identity', position = "identity", width = .75) + 
  facet_grid(Cluster ~ ., scales = 'free_y')

## Escolhendo o Número de Grupos

pct_var <- data.frame(pct_var = 0, num_clusters=2:14)
totalss <- kmeans(df, centers = 14, nstart = 50, iter.max = 100)$totss
for(i in 2:14){
  pct_var[i-1,'pct_var'] <- kmeans(df, centers = i, nstart = 50, iter.max = 100)$betweens/totalss
}

## Um Exemplo Simples

syms1 <- c('GOOGL','AMZN','AAPL','MSFT','CSCO','INTC','CVX','XOM','SLB','COP','JPM',
          'WFC','USB','AXP','WMT','TGT','HD','COST')
df <- t(sp500_px[rownames(sp500_px)>='2011-01-01', syms1])
d <- dist(df)
hcl <- hclust(d)

## O Dendograma

plot(hcl)
cutree(hcl, k = 4)

## Misturas de Normais

library(mclust)
df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM','CVX')]
mcl <- Mclust(df)
summary(mcl)
cluster <- factor(predict(mcl)$classification)
ggplot(data = df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) + 
  geom_point(alpha=.8)

summary(mcl, parameters = TRUE)$mean
summary(mcl, parameters = TRUE)$variance

## Selecionando o Número de Grupos

plot(mcl, what = 'BIC', ask = FALSE)

## Escalonando as Variáveis

df <- defaults[,c('loan_amnt','annual_inc','revol_bal','open_acc',
                  'dti','revol_util')]
km <- kmeans(df, centers=4, nstart = 10)
centers <- data.frame(size=km$size, km$centers)
round(centers, digits = 2)

df0 <- scale(df)
km0 <- kmeans(df0, centers=4, nstart = 10)
centers0 <- scale(km0$centers, center=FALSE, 
                  scale=1/attr(df0,'scaled:scale'))
centers0 <- scale(centers0, center = -attr(df0,'scaled:center'), scale = F)
data.frame(size=km0$size, centers0)

## Variáveis Dominantes

syms <- c('GOOGL','AMZN','AAPL','MSFT','CSCO','INTC','CVX','XOM','SLB','COP','JPM',
           'WFC','USB','AXP','WMT','TGT','HD','COST')
top_sp1 <- sp500_px[rownames(sp500_px)>='2005-01-01', syms]
sp_pca1 <- princomp(top_sp1)
screeplot(sp_pca1)
round(sp_pca1$loadings[,1:2],3)

## Dados Categóricos e Distância de Gower

x = defaults[1:5, c('dti','payment_inc_ratio','home','purpose')]
x

library(cluster)
daisy(x, metric = 'gower')

df <- defaults[sample(nrow(defaults), 250), c('dti','payment_inc_ratio','home','purpose')]
#d <- daisy(df, metric = 'gower')
#hcl <- hclust(d)
#dnd <- as.dendrogram(hcl)
#plot(dnd, leaflab = 'none')

df[labels(dnd_cut$lower[[1]],)]

## Problemas com Agrupamento de Dados Mistos

df <- model.matrix(~ -1 + dti + payment_inc_ratio + home + pub_rec_zero,
                   data = defaults)
df0 <- scale(df)
km0 <- kmeans(df0, centers=4, nstart = 10)
centers0 <- scale(km0$centers, center=FALSE, 
                  scale=1/attr(df0,'scaled:scale'))
scale(centers0, center = -attr(df0,'scaled:center'), scale = F)


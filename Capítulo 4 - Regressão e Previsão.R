# CAPÍTULO 4 - REGRESSÃO E PREVISÃO

## A Equqção de Regressão

options(max.print=20)

model <- lm(PEFR ~ Exposure, data = lung)

## Valores Ajustados e Resíduos

fitted <- predict(model)
resid <- residuals(model)

## Exemplo: Dados Imobiliários de King County

PSDS_PATH <- file.path('C:/Users/Ivo/Downloads/Genius/Estatistica_pratica_para_cientistas_de_dados', 'statistics-for-data-scientists')

house <- read.csv(file.path(PSDS_PATH, 'data', 'house_sales.csv'), sep = ";")

head(house[,c("AdjSalePrice","SqFtTotLiving","SqFtLot","Bathrooms",
              "Bedrooms","BldgGrade")])
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, 
               data = house, na.action = na.omit)

## Avaliando o Modelo 

summary(house_lm)

## Seleção de Modelo e Regressão Passo a Passo

house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + 
                   NewConstruction, 
               data = house, na.action = na.omit)
library(MASS)
step <- stepAIC(house_full, direction = "both")
step

## Regressão Ponderada

library(lubridate)
house$Year = year(house$DocumentDate)
house$Weight = house$Year - 2005

house_wt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, 
               data = house, na.action = na.omit)
round(cbind(house_lm=house_lm$coefficients,
            house_wt=house_wt$coefficients), digits = 3)

## Representação de Variáveis Fictícias

head(house[, 'PropertyType'])
prop_type_dummies <- model.matrix(~PropertyType -1, data = house)
head(prop_type_dummies)
lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
     Bedrooms + BldgGrade + PropertyType, data = house)

## Variáveis Fatoriais com Muitos Níveis

table(house$ZipCode)

library(dplyr)

zip_groups <- house %>% 
  mutate(resid = residuals(house_lm)) %>% 
  group_by(ZipCode) %>% 
  summarize(med_resid = median(resid),
            cnt = n()) %>% 
  arrange(med_resid) %>% 
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = ntile(cum_cnt, 5))
house <- house %>% 
  left_join(select(zip_groups, ZipCode, ZipGroup), by = 'ZipCode')

## Preditoras Correlacionadas

step_lm$coefficients
update(step_lm, . ~ . -SqFtTotLiving - SqFtFinBasement - Bathrooms)

## Variáveis de Confundimento

lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
     Bedrooms + BldgGrade + PropertyType + ZipGroup,
   data = house, na.action = na.omit)

## Interações e Efeitos Principais 

lm(AdjSalePrice ~ SqFtTotLiving*ZipGroup + SqFtLot + Bathrooms + 
     Bedrooms + BldgGrade + PropertyType, data = house, na.action = na.omit)

## Outliers

house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, data = house_98105)
sresid <- rstandard(lm_98105)
idx <- order(sresid)
sresid[idx[1]]
house_98105[idx[1], c('AdjSalePrice','SqFtTotLiving','SqFtLot','Bathrooms',
                      'Bedrooms','BldgGrade')]

## Valores Infuentes

std_resid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
hat_values <- hatvalues(lm_98105)
plot(hat_values, std_resid, cex = 10*sqrt(cooks_D))
abline(h = c(-2.5, 2.5), lty = 2)

## Heteroscedasticidade, Não Normalidade e Erros Correlacionados

df <- data.frame(resid = residuals(lm_98105),
                  pred = predict(lm_98105))
ggplot(df, aes(pred, abs(resid))) + 
  geom_point() + 
  geom_smooth()

## Gráficos Residuais Parciais e Não Linearidade

terms <- predict(lm_98105, type = 'terms')
partial_resid <- resid(lm_98105) + terms

df <- data.frame(SqFtTotLiving = house_98105[,'SqFtTotLiving'], 
                 Terms = terms[,'SqFtTotLiving'],
                 PartialResid = partial_resid[,'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) + 
  geom_point(shape=1) + scale_shape(solid=FALSE) + 
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))

## Polinomial 

lm(AdjSalePrice ~ poly(SqFtTotLiving, 2) + SqFtLot + 
     BldgGrade + Bathrooms + Bedrooms,
   data = house_98105)

## Splines

library(splines)
knots <- quantile(house_98105$SqFtTotLiving, p=c(.25, .5, .75))
lm_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots = knots, degree=3) + 
                  SqFtLot + Bathrooms + Bedrooms + BldgGrade, data = house_98105)

## Modelos Aditivos Generalizados 

library(mgcv)
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot + 
                Bathrooms + Bedrooms + BldgGrade, data = house_98105)


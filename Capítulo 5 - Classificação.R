# CAPÍTULO 5 - CLASSIFICAÇÃO

options(max.print=20)

## A Solução Naive

library(klaR)
naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_,
                          data = na.omit(loan_data))
naive_model$table

new_loan
predict(naive_model, new_loan)

## Um Exemplo Simples

library(MASS)
loan_lda <- lda(outcome ~ borrower_score + payment_inc_ratio, 
                data = loan3000)
loan_lda$scaling
pred <- predict(loan_lda)
head(pred$posterior)
lda_df <- cbind(loan3000, prob_default = pred$posterior[,'default'])
library(ggplot2)
ggplot(data = lda_df,
       aes(x = borrower_score, y = payment_inc_ratio, color = prob_default)) + 
  geom_point(alpha = .6) + 
  scale_color_gradient2(low = 'white', high = 'blue') + 
  geom_line(data = lda_df0, col = 'green', linewidth = 2, alpha = .8)

## Regressão Logística e o GLM

logistic_model

## Valores Previstos a Partir da Regressão Logística

pred <- predict(logistic_model)
summary(pred)
prob <- 1/(1 + exp(-pred))
summary(prob)

## Avaliando o Modelo

summary(logistic_model)

library(mgcv)
logistic_gam <- gam(outcome ~ s(payment_inc_ratio) + purpose_ + 
                      home_ + emp_len_ + s(borrower_score),
                    data = loan_data, family = 'binomial')
terms <- predict(logistic_gam, type='terms')
partial_resid <- resid(logistic_model) + terms
df <- data.frame(payment_inc_ratio = loan_data[,'payment_inc_ratio'],
                 terms = terms[,'s(payment_inc_ratio)'],
                 partial_resid = partial_resid[,'s(payment_inc_ratio)'])
ggplot(df, aes(x=payment_inc_ratio, y=partial_resid, solid=FALSE)) + 
  geom_point(shape=46, alpha=.4) + 
  geom_line(aes(x=payment_inc_ratio, y=terms),
            color='red', alpha=.5, size=1.5) + 
  labs(v='Partial Residual')

## Matriz de Confusão

pred <- predict(logistic_gam, newdata = train_set)
pred_y <- as.numeric(pred > 0)
true_y <- as.numeric(train_set$outcome=='default')
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)
conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)
colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat) <- c('Y = 1', 'Y = 0')
conf_mat

## Precisão, Revocação e Especificidade

# Precision
conf_mat[1,1]/sum(conf_mat[,1])
# Recall
conf_mat[1,1]/sum(conf_mat[1,])
# Specificity
conf_mat[2,2]/sum(conf_mat[2,])

## Curva ROC

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='blue') + 
  scale_x_reverse(expand=c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  geom_line(data = data.frame(x=(0:100)/100), aes(x=x, y=1-x),
            linetype = 'dotted', color = 'red')

## AUC

sum(roc_df$recall[-1]*diff(1 - roc_df$specificity))

## Undersampling

mean(loan_data$outcome == 'default')

full_model <- glm(outcome ~ payment_inc_ratio + purpose + 
                    home_ + emp_len_ + dti + revol_bal + revol_util,
                  data = train_set, family = 'binomial')
pred <- predict(full_model)
mean(pred > 0)

## Oversampling e Ponderação Acima/Abaixo

wt <- ifelse(loan_data$outcome=='default',
             1/mean(loan_data$outcome == 'default'), 1)
full_model <- glm(outcome ~ payment_inc_ratio + purpose + 
                    home_ + emp_len_ + dti + revol_bal + revol_util,
                  data = loan_data, weight = wt, family = 'binomial')
pred <- predict(full_model)
mean(pred > 0)


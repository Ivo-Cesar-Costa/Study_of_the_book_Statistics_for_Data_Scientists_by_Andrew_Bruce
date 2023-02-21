# CAPITULO 3 - EXPERIMENTOS ESTATÍSTICOS E TESTE DE SIGNIFICÂNCIA

## Exemplo: Aderência Web

ggplot(session_times, aes(x=Page, y=Time)) +
  geom_boxplot()
mean_a <- mean(session_times[session_times['Page']=='Page A','Time'])
mean_b <- mean(session_times[session_times['Page']=='Page B','Time'])
mean_b - mean_a 

perm_fun <- function(x, n1, n2)
{
  n <- n1 + n2
  idx_b <- sample(1:n, n1)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

perm_diffs <- rep(0, 100)
for(i in 1:1000) 
  perm_diffs[i] = perm_fun(session_times[,'Time'], 21, 15)
hist(perm_diffs, xlab='Session time differences (in seconds)')
abline(v= mean_b - mean_a)

## Significância Estatística e Valores P

obs_pct_diff <- 100*(200/23739 - 182/22588)
conversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 100)
for(i in 1:1000)
  perm_diffs[i] = 100*perm_fun(conversion, 23739, 22588)
hist(perm_diffs, xlab='Session time differences (in seconds)')
abline(v = obs_pct_diff)

## Valor P

mean(perm_diffs > obs_pct_diff)

prop.test(x=c(200, 182), n=c(23739, 22588), alternative = "greater")

## Testes t

t.test(Time ~ Page, data = session_times, alternative = 'less')

## Anova

library(lmPerm)
summary(aovp(Time ~ Page, data = four_sessions))

## Estatística F

summary(aov(Time ~ Page, data = four_sessions))

## Teste de Qui Quadrado: Uma Abordagem à Reamostra

chisq.test(clicks, simulate.p.value = TRUE)

## Teste de Qui Quadrado: Teoria Estatística

chisq.test(clicks, simulate.p.value = FALSE)

## Teste Exato de Fisher

fisher.test(clicks)

## Tamanho da Amostra

library(pwr)
pwr.2p.test(h = ..., n = ..., sig.level = ..., power = )
h = effect size (as a proportion)
n = sample size
sig.level = the significance level (alpha) at which the test will be conducted
power = power(probability of detecting the effect size)


#EXERCICIO 1
dados<- c(1.6,3.4,2.3,4.1,4.2,6.6,1.7,4.3)
> mean(dados)
[1] 3.525
#Como a média dessa população de aves é diferente da média da outra população de aves (2.2) 
#então elas não pertencem a mesma distribuição


#EXERCICIO 2
> local1<- c(16.3,21.2,15.0,10.8,13.9,23.5, 11.0,6.4,2.8,12.0)
> local2<- c(13.3,21.6,27.2,21.7,26.7,33.0,10.9,29.6,9.3,24.3)
#verificando se ambas tem distribuição normal
shapiro.test(local1)

Shapiro-Wilk normality test

data:  local1
W = 0.97748, p-value = 0.9503

> shapiro.test(local2)

Shapiro-Wilk normality test

data:  local2
W = 0.92794, p-value = 0.428

#Como ambos os valores de p são > 0.05 então aceitamos a hipótese nula de que ambas tem 
#distribuição normal
#Agora verifi-se se as variâncias são estatisticamente diferentes

> var.test(local1,local2)

F test to compare two variances

data:  local1 and local2
F = 0.58982, num df = 9, denom df = 9, p-value = 0.4437
alternative hypothesis: true ratio of variances is not equal to 1
95 percent confidence interval:
  0.1465029 2.3746104
sample estimates:
  ratio of variances 
0.5898196 

#valor de p > 0.05 então ace-se a Hipótese nula de que as variâncias são homogêneas
#agora testamos usando o teste T
> t.test(local1,local2,var.equal=TRUE)

Two Sample t-test

data:  local1 and local2
t = -2.6169, df = 18, p-value = 0.01747
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -15.269955  -1.670045
sample estimates:
  mean of x mean of y 
13.29     21.76 

#valor de p < 0.05 então rejeita a hipótese nula, portanto há diferenças significativas
#entre as  amostras dos 2 locais


#EXERCICIO 3
> antes<- c(5166,6080,7290,7031,6700,8908,4214,5135,5002,4900,8043,6205,8800)
> depois<- c(6310,6295,4497,5182,4273,6591,6425,4600,5407,5509,4900,5100,4900)
> shapiro.test(antes)

Shapiro-Wilk normality test

data:  antes
W = 0.93889, p-value = 0.4427

> shapiro.test(depois)

Shapiro-Wilk normality test

data:  depois
W = 0.91702, p-value = 0.2286

#valores de P maiores que 0.05 então tem distribuição normal

> var.test(antes,depois)

F test to compare two variances

data:  antes and depois
F = 3.7846, num df = 12, denom df = 12, p-value = 0.029
alternative hypothesis: true ratio of variances is not equal to 1
95 percent confidence interval:
  1.154809 12.403273
sample estimates:
  ratio of variances 
3.784628 

#valor de p<0.05 então as variâncias não são homogênias

> t.test(antes,depois,var.equal = TRUE)

Two Sample t-test

data:  antes and depois
t = 2.1672, df = 24, p-value = 0.04037
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  49.42686 2025.18852
sample estimates:
  mean of x mean of y 
6421.077  5383.769 

#valor de p menor que 0.05 então rejeita a hipótese nula e portanto há diferenças 
#nos frangos antes e depois da ração





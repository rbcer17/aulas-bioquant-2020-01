### Galhas

# Importar dados das galhas
galhas <- Planilha.galhas[,3]
plantas <- Planilha.galhas[,1]

p1 <- galhas[1:50]
p2 <- galhas[51:100]
p3 <- galhas[101:150]
p4 <- galhas[151:200]
p5 <- galhas[201:250]
p6 <- galhas[251:300]

# checando os pressupostos: teste de normalidade (distribuiÃ§ao normal)

shapiro.test(p1)
shapiro.test(p2)
shapiro.test(p3)
shapiro.test(p4)
shapiro.test(p5)
shapiro.test(p6)

# checando os pressupostos: teste de homocedasticidade (variancias homogeneas)

library(car)

leveneTest(galhas ~ plantas, data = Planilha.galhas)

# Entao vamos usar um teste de hipoteses
# Kruskal wallis (ANOVA nao parametrica)

kw=kruskal.test(Planilha.galhas$N_galhas~Planilha.galhas$Planta)

# Visto se tem diferenca ou nao, devemos verificar onde esta a diferenca
# Vamos usar um Tukey nao parametrico para o kruskal wallis
library(pgirmess)

kruskalmc(Planilha.galhas$N_galhas~Planilha.galhas$Planta)




####### Aula Ajustes de modelos

# Ajuste de modelos
# Quanto que a variável y varia em relação a variavel x
# Porém antes disso, vamos falar de:

## Correlação
# Se as variáveis estão correlacionadas, ou seja, o grau pelo qual duas 
# Variaveis tendem a mudar juntas.
# É dado pelo coeficiente de correlação "r"

# Spearman

# nao parametrico

# Diversidade de gafanhotos (y) em relação ao número de anos depois da aplicação de pesticidas (x)
x<-c(0,1,3,5,9,12,13,15,21,25)
y<-c(0,0.19,0.15,1.49,1.10,1.12,1.61,1.42,1.48,1.92)


plot(x,y,ylab = "Diversidade de gafanhotos",xlab = "Nº de anos após aplicação de pesticida")

# Parece que há uma correlação positiva entre as duas variáveis e podemos testar se esta correlação e real ou 
# nao usando a função cor.test().
# Testamos a significância da relação
cor.test(x,y,method="spearman",alternative="two.sided")


# Pearson
# parametrico

# Correlacao entre massa de peixes e comprimento dos otolitos

x <-c(6.6,6.9,7.3,7.5,8.2,8.3,9.1,9.2,9.4,10.2)
y <-c(86,92,71,74,185,85,201,283,255,222)

cor(b,c,method='pearson')





#### Regressão linear
# Note, que a pergunta não é se elas estão relacionadas, e sim como.
# EM BUSCA DA EQUAÇÃO DA RETA!!

### Exemplo: Estamos testando o crescimento de um fungo quando 
# adicionamos uma substância x no cultivo em diferentes concentrações

c1=c(11.2,11.9,13.3,10.9,8.1,7.1,13.2,2.7,6.8,16.1,11.5,14.5)
c2=c(17.4,17.6,5.4,19.3,12.2,21.5,17.4,12.4,11.8,26.9,22.8,11.4) 
c3=c(24.7,29.9,14.1,25.8,27.4,30.4,19.5,21.0,19.9,18.5,23.5,32.7) 
c4=c(21.7,29.4,50.1,37.5,33.4,26.5,45.2,35.2,17.5,38.6,35.9,33.6)
dados=c(c1,c2,c3,c4)
concentrações=c(rep(0.5,12),rep(2.5,12),rep(4.5,12),rep(6.5,12))
tabela1=data.frame(concentrações,dados)
tabela1

## Premissas

# Normalidade
shapiro.test(c1)
shapiro.test(c2)
shapiro.test(c3)
shapiro.test(c4)

# Homocedasticidade
library(lawstat)
levene.test(tabela1$dados,tabela1$concentrações)

## Fazendo a regressão, ajustando o modelo linear
regressao=lm(tabela1$dados~tabela1$concentrações)
regressao

# Verificando a significância e a interação das variáveis.
summary(regressao)
summary(aov(regressao))

# Construindo a equação da reta
# ax+b, onde a= coeficiente angular, b= intercepto
# Para encontra-los podemos olhar a funcao acima ou usar a função
# coef.
coef(regressao)
# y=3.847x+7.691

# Plotando os dados
plot(tabela1$dados~tabela1$concentrações)

# Colocando a reta de regressão
abline(regressao)

# Adicione a formula da reta de regressao ao grafico
text(2,40,"y=3.847x+7.691")

## Diagnóstico completo dos resíduos
par(mfrow=c(2,2))
plot(regressao)
# Normalidade dos resíduos
shapiro.test(residuals(regressao))
# Valores estimados de acordo com o melhor ajuste
fitted(regressao)
# Valores da diferença entre os valores estimados e observados
residuals(regressao)



### Exercicio para casa: Analise de Variancia
#Esta muito bom

## EXERCICIO 1


# Voce e um consultor ambiental e foi contratado para um determinado empreendimento.Para realizar os estudos, voce estratificou
# a area experimental em 4 estratos, e amostrou 200 arvores em cada estrato. Agora voce necessita saber se os estratos
# sao signicativamente diferentes em relacao ao volume vegetal coletado, e quais estratos diferem entre si, para que as
# analises prossigam.
# Responda essas questoes com os metodos estatisticos adequados.
# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses


# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-1.xlsx
library(readxl)
exercicio_anova_1 = read_xlsx("C:\\Users\\vinit\\Documents\\R\\Bioquant\\bioquant-mod1\\mod4\\exercicio-anova-1.xlsx")
exercicio_anova_1$dados = as.numeric(exercicio_anova_1$dados)
#estrato 1: normal.
est1=exercicio_anova_1$dados [1:200]
shapiro.test(est1)
#estrato 2: normal.
est2=exercicio_anova_1$dados [201:400]
shapiro.test(est2)
#estrato 3: normal.
est3 = exercicio_anova_1$dados[401:600]
shapiro.test(est3)
#estrato 4: normal.
est4 = exercicio_anova_1$dados[601:800]
shapiro.test(est4)
#os Dados s?o homocedasticos.
library(lawstat)
levene.test(exercicio_anova_1$dados, group = exercicio_anova_1$estratos)
#Anova: Ap?s realizar um teste anova, houve diferen?a significativa entre os estratos (alfa < 0.001).
resultado1 = aov(dados~estratos, data=exercicio_anova_1)
summary(resultado1)
#Ou seja, est? correta a hipotese de que os estratos s?o diferentes com rela??o ao volume vegetal coletado.




## EXERCICIO 2


# Em um laboratorio foi realizada a coleta de plasma sanguineo de 20 pequenos roedores, e o grupo de pesquisa quer
# saber se determinado hormonio afeta o nivel de plasma sanguineo, se ha diferenca entre esse nivel para machos e 
# femeas e se existe diferenca entre machos e femeas na presenca de hormonio e/ou na ausencia dele.
# Responda as questoes levantadas utilizando metodos estatisticos adequados


# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-2.xlsx


# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses


# OBS 2: Na coluna "tratamentos", o numero 1 significa  "presenca do hormonio" e o numero 2"ausencia do hormonio";
# Na coluna "sexo", o numero 1 significa "macho" e o numero 2 "femea".

library(readxl)
exercicio_anova_2=read_xlsx("C:\\Users\\vinit\\Documents\\R\\Bioquant\\bioquant-mod1\\mod4\\exercicio-anova-2.xlsx")
exercicio_anova_2$Plasma = as.numeric(exercicio_anova_2$Plasma)
exercicio_anova_2$Tratamento = as.character(exercicio_anova_2$Tratamento)
View(exercicio_anova_2)
#Normalidade do tratamento: os dados s?o normais para ambos os tratamentos
trat1=c(exercicio_anova_2$Plasma [1:10])
trat2 = c(exercicio_anova_2$Plasma [11:20])
shapiro.test(trat1)
shapiro.test(trat2)

#Normalidade do sexo: os dados s?o normais para ambos os sexos.
shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="1",]$Plasma)
shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="2",]$Plasma)

#Homocedasticidade:
library(lawstat)
levene.test(exercicio_anova_2$Plasma, group = exercicio_anova_2$Sexo)
levene.test(exercicio_anova_2$Plasma, group =,exercicio_anova_2$Tratamento)

#Twoway anova: s? foi encontrado diferen?a significativa (alfa <0.001) entre o n?vel do plasma e a presen?a do hormonio.
resultado2 = aov(Plasma~ Tratamento+Sexo+Tratamento:Sexo, data = exercicio_anova_2)
summary(resultado2)

#Isso confirma a hipotese de que o n?vel do plasma varia de acordo com a presen?a do hormonio.
#porem tanto o plasma quanto a presen?a do hormonio n?o variaram de acordo com o sexo (alfa > 0.05).
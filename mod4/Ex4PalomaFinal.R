#########
### Exercicio para casa: Analise de Variancia


## EXERCICIO 1


# Voce e um consultor ambiental e foi contratado para um determinado empreendimento.Para realizar os estudos, voce estratificou
# a area experimental em 4 estratos, e amostrou 200 arvores em cada estrato. Agora voce necessita saber se os estratos
# são signicativamente diferentes em relacao ao volume vegetal coletado, e quais estratos diferem entre si, para que as
# analises prossigam.
# Responda essas questoes com os metodos estatisticos adequados.
# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses


# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-1.xlsx



######################################################################

install.packages("openxlsx")
library(readxl)
exanova1<- read_excel("exanova1.xlsx")

#########

View(exanova1)

##############

exanova1 $ dados=as.numeric(exanova1 $ dados)

Estrato1 = exanova1 $ dados [1:200]

Estrato2 = exanova1 $ dados [201:400]

Estrato3 = exanova1 $ dados [401:600]

Estrato4 = exanova1 $ dados [601:800]



##### Os dados s?o normais pois todos os estratos p>0,05 #####

shapiro.test(Estrato1)

shapiro.test(Estrato2)

shapiro.test(Estrato3)

shapiro.test(Estrato4)



### H0, logo a vari?ncia ? homosced?stica ###


install.packages("lawstat")
library(lawstat)
levene.test(exanova1$dados, group = exanova1$estratos)



###Sim, s?o signicativamente diferentes em relacao ao volume vegetal coletado, se consideramos signific?ncia de p<0,05, com Pr(>F)<2e-16###

resultado <- aov(dados ~ estratos, data = exanova1)
summary(resultado)




######################################################################


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




install.packages("openxlsx")
library(readxl)
exanova2<- read_excel("exanova2.xlsx")

View (exanova2)
######


Tratamento1 =  exanova2$Plasma [1:10]

Tratamento2 = exanova2$Plasma [11:20]

###S?o Normais####

shapiro.test(Tratamento1)

shapiro.test(Tratamento2)


####S?o normais #####

shapiro.test(exanova2[Sexo = "1"]$Plasma)

shapiro.test(exanova2[Sexo = "2"]$Plasma)


#########
install.packages("lawstat")
library(lawstat)

#### H1, logo a vari?ncia ? heteroced?stica
levene.test(exanova2$Plasma, group = exanova2$Tratamento)

###H0, logo a vari?ncia ? homoced?stica
levene.test(exanova2$Plasma, group = exanova2$Sexo)


#### Para que a ANOVA seja feita ? nescess?rios os pressupoostos de normalidade e homocedasticidade#####

resultado2 <- aov(Plasma ~ Tratamento+Sexo+Tratamento:Sexo, data = exanova2)
summary(resultado2)











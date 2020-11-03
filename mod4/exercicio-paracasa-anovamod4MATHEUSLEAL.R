### Exercicio para casa: Analise de Variancia


## EXERCICIO 1


# Voce e um consultor ambiental e foi contratado para um determinado empreendimento.Para realizar os estudos, voce estratificou
# a area experimental em 4 estratos, e amostrou 200 arvores em cada estrato. Agora voce necessita saber se os estratos
# são signicativamente diferentes em relacao ao volume vegetal coletado, e quais estratos diferem entre si, para que as
# analises prossigam.
# Responda essas questoes com os metodos estatisticos adequados.
# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses

library(readxl)
exercicio_anova_1 <- read_excel("mod4/exercicio-anova-1.xlsx")

# Neste caso vamos usar anova one way pois estamos analisando um unico fator(uma única váriavel independente); 
# Temos mais de duas amostras e queremos saber se existe diferencia real 
# entre as medias. 

#Dividindo os dados em 4 grupos.
estrato1 <- exercicio_anova_1$dados [1:200]
estrato2 <- exercicio_anova_1$dados [201:400]
estrato3 <- exercicio_anova_1$dados[401:600]
estrato4 <- exercicio_anova_1$dados[601:800]

# Criando as variaveis dependentes
Varidep <- c(estrato1, estrato2, estrato3, estrato4)

# Criar as variaveis independentes
Variindep <- c(rep("estrato1", 200), rep("estrato2", 200), rep("estrato3", 200), 
         rep("estrato4", 200))

# data.frame
Estratos <- data.frame(Variindep, Varidep)

# Premissas
# 1. Normalidade
shapiro.test(estrato1)
shapiro.test(estrato2)
shapiro.test(estrato3)
shapiro.test(estrato4)
#Estão distribuidos normalmente.

# 2. Homocedasticidade
# Usaremos o test de levene que calcula a homocedasticidade
# H0 = variancia entre os grupos igual
# levene.test(y = vetor numerico, group = fator dos dados)
install.packages("lawstat")
library(lawstat)
levene.test(Estratos$Varidep, group = Estratos$Variindep)
#A variancia entre os grupos é igual.

# 3. Independencia das amostras
#As amostras foram retiradas de 4 estratos diferentes, portanto são independentes.

# aov(formula = y variando em relacao a X, data = tabela)
resultado <- aov(Varidep ~ Variindep, data = Estratos)
summary(resultado)

#O resultado foi F > 1 com um P valor de 2e-16, se coniderarmos um valor de significancia de 0,05 rejeitaremos H0, ou seja
#as médias são diferentes.


## EXERCICIO 2


# Em um laboratorio foi realizada a coleta de plasma sanguineo de 20 pequenos roedores, e o grupo de pesquisa quer
# saber se determinado hormonio afeta o nivel de plasma sanguineo, se ha diferenca entre esse nivel para machos e 
# femeas e se existe diferenca entre machos e femeas na presenca de hormonio e/ou na ausencia dele.
# Responda as questoes levantadas utilizando metodos estatisticos adequados

#Neste caso usaremos a anova two way, pois temos dois fatores para analisar.

library(readxl)
exercicio_anova_2 <- read_excel("mod4/exercicio-anova-2.xlsx")

#Variavel dependente

Plasma <- exercicio_anova_2$Plasma

#Variaveis independentes

Tratamento <- c(rep("presença", 10) , rep("ausência", 10))

Sexo <- c(rep("macho",5), rep("femea",5), rep("macho",5), rep("femea",5))

# Data.frame
Tabela <- data.frame(Sexo, Tratamento, Plasma)
View(Tabela)

# 1. Normalidade Tratamento

shapiro.test (Tabela[Tabela$Tratamento == "presença",]$Plasma)
shapiro.test (Tabela[Tabela$Tratamento == "ausência",]$Plasma)

#dados apresentam distribuição normal considerando 0,05 de significancia

# 1. Normalidade Sexo

shapiro.test (Tabela[Tabela$Sexo == "macho",]$Plasma)
shapiro.test (Tabela[Tabela$Sexo == "femea",]$Plasma)

#dados apresentam distribuição normal considerando 0,05 de significancia

# 2. Homocedasticidade
levene.test(Tabela$Plasma, group = Tabela$Tratamento)

#H1 variancia não é homogenea considerando 0,05 de significancia

levene.test(Tabela$Plasma, group = Tabela$Sexo)

#H0 variancia é homogenea considerando 0,05 de significancia

#Visto a heterocedasticidade esta analise de variância é inválida.

# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses


# OBS 2: Na coluna "tratamentos", o numero 1 significa  "presenca do hormonio" e o numero 2"ausencia do hormonio";
# Na coluna "sexo", o numero 1 significa "macho" e o numero 2 "femea".
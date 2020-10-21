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
#Importamos a planilha com os dados ao clicar com o botão esquerdo no arquivo do projeto e selecionarmos a opção "import dataset"

#Nossa hipótese nula é de que os estratos não diferem entre si e pertecem a mesma população estatística. 
#Para testar essa hipótese vamos fazer uma ANOVA, entretanto, é necessário testar as premissas para esse teste, que são a interdependência das amostras, homocedascidade e distribuição normal. 
#Vamos selecionar os dados desejados criando objetos. Escolhemos os dados que queremos utilizar, no caso a planilha "exercicio_anova_1" e indicamos a coluna que queremos extrair com o "$". 
estrato1 <- as.numeric(exercicio_anova_1$dados[1:200])
estrato2 <- as.numeric(exercicio_anova_1$dados[201:400])
estrato3 <- as.numeric(exercicio_anova_1$dados [401:600])
estrato4 <- as.numeric(exercicio_anova_1$dados[601:800])
#Vamos testar a premissa da normalidade dos dados por meio de um shapiro test
shapiro.test(estrato1)
shapiro.test(estrato2)
shapiro.test(estrato3)
shapiro.test(estrato4)
#Como todos os valores de p do teste são < 0,5 rejeitamos a hipótese nula, e aceitamos a hipótese que todas as distribuições são normais. 
#Agora precisamos testar a premissa da homocedascidade das amostras por meio de um teste de levene. 
install.packages("lawstat")
library(lawstat)
dados <- as.numeric(exercicio_anova_1$dados)
estratos <- (exercicio_anova_1$estratos)
dados <- as.numeric(exercicio_anova_1$dados)
levene.test(dados, group= exercicio_anova_1$estratos)
#Como o valor de p é <0.5, rejeitamos a hipótese nula e aceitamos a hipótese que as amostras são homoscedásticas, isto é. as variâncias são homogêneas.
#Agora, que já sabemos que as amostras são indepententes, possuem distrbuição normal e são homoscedásticas, podemos realizar a ANOVA
resultado <- aov(dados~exercicio_anova_1$estratos, data = exercicio_anova_1)
summary(resultado)
#Como o falor e F > 1, rejeitamos a hipótese nula e concluímos que os estratos são significativamente em relação ao volume vegetal coletado. 
## EXERCICIO 2


# Em um laboratorio foi realizada a coleta de plasma sanguineo de 20 pequenos roedores, e o grupo de pesquisa quer
# saber se determinado hormonio afeta o nivel de plasma sanguineo, se ha diferenca entre esse nivel para machos e 
# femeas e se existe diferenca entre machos e femeas na presenca de hormonio e/ou na ausencia dele.
# Responda as questoes levantadas utilizando metodos estatisticos adequados


# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-2.xlsx
#Para realizar a ANOVA é necessãrio testar a normalidade dos dados. 

#Vamos chamar de plasma1 os dados do tratamento 1 e plasma2 os dados do tratamento 2. 
plasma <- as.numeric(exercicio_anova_2$Plasma)
plasma1 <- c(plasma[1:10])
plasma2 <- c(plasma[11:20])
shapiro.test(plasma1)
shapiro.test(plasma2)
#Devemos tamém checar a normalidade da distribuição entre os sexos
shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="1",]$Plasma)
shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="2",]$Plasma)
#como o valor de p < 0.5 confirmamos que a distribuição é normal.
#para testar a homoscedascidade dos dados, faremos o teste de levene 
library(lawstat)
tratamento <-  as.character(exercicio_anova_2$Tratamento)
tratamento1 <- c(tratamento[1:10])
tratamento2 <- c(tratamento[11:20])
levene.test(plasma, group = exercicio_anova_2$Sexo)
levene.test(plasma, group =,tratamento)
#como o valor de p <0.5,concluímos que as variâncias são homogêneas. 
#Como estamos analisando dois fatores, é preciso realizar uma TWO WAY ANOVA.Para analisar a interação entre o tratamento e o sexo utilizamos o símbolo ":". e para analisar o fator tratamento e o fator sexo utilizamos o símbolo "+"  
resultado2 <- aov(plasma~ tratamento+Sexo+tratamento:Sexo, data = exercicio_anova_2)
summary(resultado2)
#Como o valor de F para a interação dos sexos com o tratamento foi <1, aceitamos a hiṕotese nula de que não há diferença significativa entre machos e fêmeas no nível de plasma sanguíneo. 
#Como o valor de F para sexo foi >1, rejeitamos a hipótese nula e confirmamos que o tratamento modifica o plasma sanguíneo significativamente
# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses


# OBS 2: Na coluna "tratamentos", o numero 1 significa  "presenca do hormonio" e o numero 2"ausencia do hormonio";
# Na coluna "sexo", o numero 1 significa "macho" e o numero 2 "femea".
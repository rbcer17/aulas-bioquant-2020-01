#### Exerc?cio 1

# O professor Roberto Cavalcanti foi ao campo e coletou o comprimento do corpo de 5 aves e tambem o comprimento das asas
# destes individuos. Ele quer saber qual a relacao entre essas variaveis e quanto uma varia em funcao da outra.
# Ajude-o fazendo o teste estatistico adequado

asas=c(1.6, 1.5, 1.8, 1.3, 1.2 ,1.8, 1.5, 1.6, 1.7, 1.8, 1.4, 1.3, 1.8, 1.6, 1.7, 1.5, 1.5, 1.1, 2.0 ,1.7, 1.9, 1.6, 1.7, 1.6, 1.4, 1.5, 1.6, 1.5,1.6, 1.7, 1.5, 1.6, 1.6, 1.3, 1.4, 1.5, 1.5, 1.4 ,1.6 ,1.5, 1.4 ,1.5, 1.8 ,1.1, 1.7, 1.4, 1.1, 1.7, 1.6, 1.4)
corpo= c(3.3, 1.8, 4.2, 3.3, 1.3, 1.3, 2.5, 2.3, 4.4, 3.6, 2.3, 2.8, 4.4, 2.1, 2.3, 2.2, 4.3, 4.0, 1.0, 3.4, 2.6, 3.0, 3.1, 3.1, 2.9, 2.6, 3.0, 3.2,3.8, 2.6, 2.0, 2.0, 3.4, 4.0, 2.5, 2.0, 2.2, 4.5, 2.7, 2.1, 3.1 ,3.8,2.6, 2.8, 3.3, 4.6, 2.8, 2.9, 2.3, 2.8)
#Fazemos um shapiro test para garantir a normalidade das amostras 
shapiro.test(asas)
shapiro.test(corpo)
#Como os valores de p para os dois conjuntos de dados foi menor do que 0.5, aceitamos a hipotése nula de que os dados são normais
help("cor.test")
#Para estimar a associação entre os dois conjuntos de dados, aplicaremos a função cor.teste usando o coeficiente de Pearson
cor.test(corpo,asas,method="pearson",alternative="two.sided")
# Como o valor obtido é diferente de 0, aceitamos a hipótese alternativa que há uma associação entre as amostras 
#Agora, faremos uma regressão ajustando o modelo linear 
regressao=lm(asas~corpo)
regressao
summary(regressao)
summary(aov(regressao))
plot(asas~corpo)
abline(regressao)
#Como indicado na aov, e observado no gráfico as amostras não se ajustam ao modelo linear. Isso indica que as amostras não são correlacionadas
### Exercicio 2

# Um jornal sensacionalista anda dizendo que o aumento da frequencia media de sorvetes vendidos em uma praia esta causando aumento no ataque
# de tubarao na mesma praia, ambos no periodo de. Verifique tal informacao e discuta a possibilidade da relacao causal sugerida procede, uma
# vez que o resultado for confirmado. O que pode explicar a afirmacao dada, se as variaveis forem correlatas? Discorra
# sobre o porque da utilizacao tecnica estatistica escolhida para verificar tal informacao, dentre as quais foram vistas na ultima aula,
# em detrimento da outra.

sorvete=c(20,24,24,26,27,27,38,36,39,21,20,19)
tubaroes=c(0,0,0,0,0,1,5,6,9,1,0,0)
#Para testar a normalidade das amostras, fazemos um teste de shapiro 
shapiro.test(sorvete)
shapiro.test(tubaroes)
#Para verificar se há associação entre as amostras faremos um cor.tes usando o coeficiente de pearson 
cor.test(sorvete, tubaroes, method = "pearson", alternative="two.sided")
#Como o valor obtido é positivo e diferente de 0, inferimos que as amostras possuem uma associação entre si
#Agora faremos a regressão ajustando o modelo linear 
regressao2=lm(tubaroes~sorvete)
regressao2
summary(regressao2)
summary(aov(regressao2))
coef(regressao2)
plot(sorvete~tubaroes)
abline(regressao2)
#Apesar dos testes indicarem que há uma correlação entre as amostras é possivel que as variaveis não estejam variando uma em função da outra, mas a uma outra variável, como por exemplo, a temperatura no dia. 

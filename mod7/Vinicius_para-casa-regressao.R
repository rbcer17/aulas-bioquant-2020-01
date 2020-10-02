#### Exerc?cio 1

# O professor Roberto Cavalcanti foi ao campo e coletou o comprimento do corpo de 5 aves e tambem o comprimento das asas
# destes individuos. Ele quer saber qual a relacao entre essas variaveis e quanto uma varia em funcao da outra.
# Ajude-o fazendo o teste estatistico adequado
asas = c(1.6, 1.5, 1.8, 1.3, 1.2 ,1.8, 1.5, 1.6, 1.7, 1.8, 1.4, 1.3, 1.8, 1.6, 1.7, 1.5, 1.5, 1.1, 2.0 ,1.7, 1.9, 1.6, 1.7, 1.6, 1.4, 1.5, 1.6, 1.5, 1.6, 1.7, 1.5, 1.6, 1.6, 1.3, 1.4, 1.5, 1.5, 1.4 ,1.6 ,1.5, 1.4 ,1.5, 1.8 ,1.1, 1.7, 1.4, 1.1, 1.7, 1.6, 1.4)
corpo = c(3.3, 1.8, 4.2, 3.3, 1.3, 1.3, 2.5, 2.3, 4.4, 3.6, 2.3, 2.8, 4.4, 2.1, 2.3, 2.2, 4.3, 4.0, 1.0, 3.4, 2.6, 3.0, 3.1, 3.1, 2.9, 2.6, 3.0, 3.2,3.8, 2.6, 2.0, 2.0, 3.4, 4.0, 2.5, 2.0, 2.2, 4.5, 2.7, 2.1, 3.1 ,3.8,2.6, 2.8, 3.3, 4.6, 2.8, 2.9, 2.3, 2.8)

shapiro.test(asas)
shapiro.test(corpo)
plot(corpo~asas)

regressao = lm(asas~corpo)
summary(regressao)
summary(aov(regressao))

coef(regressao)
plot(asas~corpo)
abline(regressao)

cor.test(asas,corpo, method = "spearman", alternative = "two.sided")
# De acordo com o resultado do teste anova, é possivel afirmar que as duas variáveis não são corelacionadas (alfa > 0.05).

### Exercicio 2

# Um jornal sensacionalista anda dizendo que o aumento da frequencia media de sorvetes vendidos em uma praia esta causando aumento no ataque
# de tubarao na mesma praia, ambos no periodo de. Verifique tal informacao e discuta a possibilidade da relacao causal sugerida procede, uma
# vez que o resultado for confirmado. O que pode explicar a afirmacao dada, se as variaveis forem correlatas? Discorra
# sobre o porque da utilizacao tecnica estatistica escolhida para verificar tal informacao, dentre as quais foram vistas na ultima aula,
# em detrimento da outra.

sorvete = c(20, 24, 24, 26, 27, 27, 38, 36, 39, 21, 20, 19)
tubaroes = c(0, 0, 0, 0, 0, 1, 5, 6, 9, 1, 0, 0)

shapiro.test(sorvete)
shapiro.test(tubaroes)

regressao2 = lm(tubaroes~sorvete)
summary(regressao2)


plot(tubaroes~sorvete)
abline(regressao2)
coef(regressao2)

cor.test(tubaroes, sorvete, method = "spearman", alternative = "two.sided")

# De acordo com o resultado do teste de spearman, os dois valores podem ser correlacionados (alfa < 0,05).
# Essa correlação pode ter ocorrido aleatoreamente, visto que é frequente que duas medidas possuam uma correlação sómente númerica, 
# não tendo então uma correlação realmente causal e biologica.
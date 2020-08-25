#Primeiro exercicio de Biologia Quantitativa
#O simbolo jogo da velha e usado para indicar linha de comentario
#arquivo da Paloma
#se houver o simbolo jogo da velha a linha e ignorada pelo R
#O R tem um conjunto de dados exemplo chamado trees
#Vamos visualizar:
View(trees)
#Agora vamos buscar os dados sumarizados
summary(trees)
#Exercicio para os alunos:
#use o comando plot do R para visualizar os dados deste conjunto de dados
#use os comandos de media e variancia para obter a media e variancia da
#altura, volume e diametro das 30 arvores
#grave o script com um novo nome e extensao .R
#sincronize com o seu repositorio no github e faca um pull request para 
#o repositorio mestre
#edite o documento do teams fazendo o seu passo a passo

## Exercicio Paloma ##

## Vizualizar Dados ##
View(trees)

## Listar dados sumarizados ##
summary(trees)

## Fazendo o Plot do grafico ##
plot(trees)
## MEDIA ##
mean(trees$Girth)
mean(trees$Height)
mean(trees$Volume)

## Variancia ##
var(trees$Volume)
var(trees$Girth)
var(trees$Height)

##FIM#


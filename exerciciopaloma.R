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

## Exercício Paloma ##

## Vizualizar Dados ##
View(trees)

## Listar dados sumarizados ##
summary(trees)

## Fazendo o Plot do gráfico ##
plot(trees)
## Para obter a média usamos o comando mean encontrado no packages do R
## MÉDIA ##
mean(trees$Girth)
mean(trees$Height)
mean(trees$Volume)

## Para obter a variância usamos o comando var encontrado no packages do R,
# e usamos o comando cifrão $ para designar  de qual conjunto de dados,
# queremos extrair o comando seja ele de média, variância e etc.

## Variância ##
var(trees$Volume)
var(trees$Girth)
var(trees$Height)

##FIM#


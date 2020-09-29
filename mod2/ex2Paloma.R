##Tabela Exercício 2 no formato (.xlsx) ##
## Depois de ter formatado a tabela de acordo com o exercício no Excel.##
##Instalei o pacote no R para ler arquivo no formato padrão do excel.##

install.packages("openxlsx")

## executei o comando para ler tabelas##
library(tables)

##Depois executei o comando para saber onde está a área de trabalho do R no meu computador##

getwd()

##Realizei o comando para ler a tabela## 

ex2Paloma <-read.xlsx("C:/Users/iandr/OneDrive/Documentos/ex2Paloma.xlsx")
   
## Por fim realizei o comando para visualizar a tabela##
View(ex2Paloma)
 


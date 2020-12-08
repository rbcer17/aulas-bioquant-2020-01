# Antes de começar faça o download do script e das tabelas e salve-os na mesma pasta,
# mantendo os nomes das tabelas.

library(readxl)
library(lawstat)
library(ggplot2)
likes = read_excel("Likes.xlsx")
comp = read_excel("Compartilhamentos.xlsx")
soc_bio = read_excel("Soc_Bio.xlsx", col_types = c("text", "numeric"))

# concatenando:
View(soc_bio)
View(comp)
View(likes)
desm_l = c(likes$Likes[1:15])
sab_l = c(likes$Likes[16:27])
sep_l = c(likes$Likes[28:42])
vc_l = c(likes$Likes[43:57])
desm_c = c(comp$Compartilhamentos[1:15])
sab_c = c(comp$Compartilhamentos[16:27])
sep_c = c(comp$Compartilhamentos[28:42])
vc_c = c(comp$Compartilhamentos[43:57])
soc = c(soc_bio$Likes[1:28])
bio = c(soc_bio$Likes[29:56])

# desvios padrões.
sdl = c(sd(desm_l), sd(sab_l), sd(sep_l), sd(vc_l))
sda = c(sd(bio), sd(soc))

# normalidade: sab_l, sep_l, sab_c e bio
shapiro.test(desm_l)
shapiro.test(sab_l)
shapiro.test(sep_l)
shapiro.test(vc_l)
shapiro.test(desm_c)
shapiro.test(sab_c)
shapiro.test(sep_c)
shapiro.test(vc_c)
shapiro.test(soc)
shapiro.test(bio)

# Normalizar
normdesml =log(desm_l)
normvcl = log(vc_l)
normdesmc = log(desm_c)
normsoc = log(soc)
normsepl = log(sep_l)

# teste de normalidade 2:
shapiro.test(normdesml)
shapiro.test(normvcl)
shapiro.test(normdesmc)
shapiro.test(normsoc)
shapiro.test(normsep)
#Homocedasticidade: ambos são homocedasticos
levene.test(likes$Likes, group = likes$Posts)
levene.test(comp$Compartilhamentos, group = comp$Posts)
levene.test(soc_bio$Likes, group = soc_bio$Posts)

#testes likes:
resu_l=aov(normdesml~normvcl+sep_l+normvcl:sep_l)
summary(resu_l)

#sabados:
t.test(sab_l, sep_l)
wilcox.test(sab_l, desm_l)
wilcox.test(sab_l, vc_l)

#testes compartilhamentos:
#desmitificando:
kruskal.test(desm_c~sep_c)
kruskal.test(desm_c~vc_c)
#sabados:
wilcox.test(sab_c, sep_c)
wilcox.test(sab_c, vc_c)
wilcox.test(sab_c, desm_c)
#se proteja:
kruskal.test(sep_c~vc_c)

#Teste assuntos:
result = aov(normsoc~bio)
summary(result)

#graficos:
#media likes
xlik = aggregate(likes$Likes, by=list(likes$Posts), FUN=mean)
colnames(xlik) = c("Posts", "Likes")
xlik = xlik[order(xlik$Posts), ]
head(xlik, 4)
xlik$sd = sdl
View(xlik)
#grafico like:
theme_set(theme_bw())
ggplot(xlik, aes(x=Posts, y=Likes)) +
  geom_bar(stat = "identity", width =0.3, fill="gray", colour = "black") +
  labs(title="Meu corpo eu cuido",
       subtitle = "Likes por posts",
       caption = "Fonte: @meucorpoeucuido") + 
  coord_cartesian(ylim = c(0,35), expand = c(0,0)) +
  annotate("text", x=1, y=33, label="a", size=6) +
  annotate("text", x=2, y=19.75, label="b", size=6) +
  annotate("text", x=3, y=24.75, label="ab", size=6) +
  annotate("text", x=4, y=28.5, label="b", size=6) +
  geom_errorbar(aes(ymin=Likes-sd, ymax=Likes+sd), width=0.15,
                position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

#media assuntos
xsoc = aggregate(soc_bio$Likes, by=list(soc_bio$Posts), FUN=mean)
colnames(xsoc) = c("Assunto", "Likes")  
xsoc = xsoc[order(xsoc$Assunto), ]
head(xsoc)
xsoc$sd = sda
View(xsoc)

#grafico assunto
theme_set(theme_bw())
ggplot(xsoc, aes(x=Assunto, y=Likes)) +
  geom_bar(stat = "identity", width=0.4, fill="gray", colour = "black") +
  labs(title = "Meu corpo eu cuido",
       subtitle = "Tipo de assunto por likes",
       caption = "Fonte: @meucorpoeucuido") +
  coord_cartesian( xlim = c(3,0), ylim = c(0,35), expand = c(0,0)) +
  annotate("text", x=1, y=25, label="a", size=6) +
  annotate("text", x=2, y=30, label="a", size=6) +  
  geom_errorbar(aes(ymin=Likes - sd,
                    ymax=Likes + sd), width=.2,
                position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(angle=65,vjust=0.5))

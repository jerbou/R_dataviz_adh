 # =========== dataviz adhesion =============

# ==== 00 : chargement de librairie ====
library(dplyr)
library(XML)
library(plyr)
library(xmlparsedata)
library(data.table)
library(dtplyr)
library(ggplot2)
library(rjson)
library(XLConnect)
library(xlsimple)
library(RODBC)
library(xlsx)
library(data.table)
library(readr)
library(sqldf)
library(stringr)
library(tidyr)
library(readODS)
library(dismo)
library(XML)
library(stringi)
library(httr)
library(doBy)
library(scales)
library(banR)
library(readr)
library(readxl)
library(gridExtra)
library(forcats)
library(officer)
library(mschart)
library(rvg)

# ==== 01 : chargement du workdir ====
# setwd('T:/Géobourgogne/DONNEES/PLATEFORME/ENTRANTE/ternum/')
setwd('C:/COPY_data_local/ternum/')

# df0 <- read_excel("T:/Géobourgogne/DONNEES/PLATEFORME/ENTRANTE/ternum/Liste adhérents pour météo_03_2018.xlsx")
# df0 <- read_excel("C:/COPY_data_local/ternum/Liste adhérents pour météo_03_2018.xlsx")
df0 <- read_excel("C:/COPY_data_local/ternum/Liste adhérents pour météo_04_2018.xlsx")
# === Reverse order tips ====
# https://stackoverflow.com/questions/42710056/reverse-stacked-bar-order


# ==== 02 : reconstruction de la typologie nature adhérent ====
df0[c("nature_ad")] <- NA
# df1 <- transform(df1,"samedi"= ifelse(jour4_f=="samedi" & jour4_o=="lundi" , lundi, samedi))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==1, 'Membres fondateurs', NA))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' >=2 &  df0$'collège' <=5 | df0$'collège' >=14 & df0$'collège' <=17, 'Communes de moins de 3500 hab' , nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==6, 'Communes de 3500 a 2000 hab', nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==7, 'Communes plus de 20000 hab', nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==8, 'Groupements de collectivités', nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==9, 'Syndicat intercommunaux', nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==10, 'Etablissement Recherche', nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==11, 'Etablissement Santé', nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==12, 'Sanitaires et Protection civile', nature_ad))
df0 <- transform(df0, "nature_ad" = ifelse(df0$'collège' ==13, 'Divers', nature_ad))

# renommer le champ
setnames(df0, old = 2, new = c('college'))

# ==== 03 subset des adhrents ====
df_ad <- subset(df0, df0$'college' != 1)
cat <- sapply(df_ad, is.factor)
df_ad[cat] <- lapply(df_ad[cat], factor)

View(df0)

# ==== 04 on contruit un dataframe pour le nomnre d adh par date de delib ====
df0$mois <- format(as.Date(df0$datedélib), "%Y-%m")
bil_mois <- summaryBy(libelle ~ mois , data = df0, FUN=c(length), fun.names=("nombre"))
head(bil_mois)

bil_mois2 <- summaryBy(libelle ~ mois+CodeDepartement , data = df0, FUN=c(length), fun.names=("nombre"))


bil_mois$somcum <- cumsum(bil_mois$libelle.nombre)

ggplot(data=bil_mois, aes(x = factor(bil_mois$'mois'), y=bil_mois$somcum )) +geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 1))

ggplot(data=bil_mois, aes(x = factor(bil_mois$'mois'), y=bil_mois$libelle.nombre )) +geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(data=bil_mois2, aes(x = factor(bil_mois2$'mois'), fill=as.factor(bil_mois2$CodeDepartement))) +geom_bar()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# tableau recap et GRAPHE
bil_nat <- summaryBy(cotisation_annuelle  ~ nature_ad , data = df0, FUN=c(sum,mean,length), fun.names=c('cotis_som','cotis_mean', 'cotis_nb'))
bil_col <- summaryBy(cotisation_annuelle  ~ df0$college , data = df0, FUN=c(sum,mean,length), fun.names=c('cotis_som','cotis_mean', 'cotis_nb'))
bil_col$percent = round((bil_col$cotisation_annuelle.cotis_som / sum(bil_col$cotisation_annuelle.cotis_som)) * 100 , 1)

h3 <- ggplot(data=bil_col, aes(x = factor(bil_col$'college'), y=bil_col$cotisation_annuelle.cotis_som )) +geom_bar(stat="identity") 
h4 <- h3 + labs(fill= "Département", x="Collège", y = "nombre")
h4 + geom_text(size = 4, color = "white", stat='identity', aes(label=(paste(as.character(percent),"%",sep=' '))), position = position_stack(vjust = 0.5))
# reordonner par frequence
bil_col$college <- factor(bil_col$college, levels = bil_col$college[order(-bil_col$cotisation_annuelle.cotis_som)])
h3 <- ggplot(data=bil_col, aes(x = factor(bil_col$'college'), y=bil_col$cotisation_annuelle.cotis_som )) +geom_bar(stat="identity") 
h4 <- h3 + labs(fill= "Département", x="Collège", y = "nombre")
h4 + geom_text(size = 4, color = "white", stat='identity', aes(label=(paste(as.character(percent),"%",sep=' '))), position = position_stack(vjust = 0.5))


# resume par group
# https://stackoverflow.com/questions/9847054/how-to-get-summary-statistics-by-group
# http://stat545.com/block024_group-nest-split-map.html
# tapply(df0$college, df0$cotisation_annuelle, function(x) format(summary(x), scientific = TRUE))




df01 <- df0
# df0$college


View(df_ad)
recap <- summaryBy(cotisation_annuelle + NbreHabitants ~ nature_ad , data = df_ad, FUN=c(sum,mean,length))


# source pour presentation markdonw
# http://www.hafro.is/~einarhj/education/tcrenv2016/pre/r-markdown-example.Rmd

# on va distinguer les data
df_ad <- subset(df0, df0$'collège' != 1)
cat <- sapply(df_ad, is.factor)
df_ad[cat] <- lapply(df_ad[cat], factor)

df_ad_bg <- dplyr::select(df_ad, -3)
# cat <- sapply(df_ad_bg, is.factor)
# df_ad_bg[cat] <- lapply(df_ad_bg[cat], factor)


unique(df0$nature_ad)

View(head(df0,150))
View(tail(df0,150))

# dataviz
# pour mettre plusieurs graphes
# http://www.sthda.com/french/wiki/ggplot2-combiner-plusieurs-graphiques-sur-la-m-me-page-logiciel-r-et-visualisation-de-donn-es
h0 <- ggplot(data=df0, aes(x = factor(df0$'college'))) +geom_bar(stat = "count")
             # + scale_y_continuous(breaks=seq(0,300,10)) + scale_x_continuous(breaks=seq(0,2090000,50000)) + theme(axis.text.x = element_text(angle=90, vjust=0.5)) + ggtitle("Nombre de marchÃ© selon \n leur montant et par expÃ©rimentateur, 17/08/2017")
# h0 + labs(fill= "Type local", x="Collège", y = "nombre") + geom_text(stat='count', aes(label=..count..), vjust=1.5)

h0 + labs(fill= "Type local", x="Collège", y = "nombre") + geom_text(color = "white", stat='count', aes(label=..count..), vjust=1.5)

# ==== ordonner
# https://stackoverflow.com/questions/46554429/ggplot2-reordering-special-variable-count
# https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
# http://www.reed.edu/data-at-reed/resources/R/reordering_geom_bar.html

df1 <- df0
# df1$'collège' <- fct_infreq(df1$'collège') ERROR
df1$college <- factor(df1$college, levels = df1$college[order(-df1$..count..)])
# https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
# https://stackoverflow.com/questions/42497063/r-sorting-factor-by-level-frequency-and-plotting

# df$Var1 <- reorder(df$Var1,df$Var1,FUN=length)
# levels(df$Var1)

df1$college <- reorder(df1$college, df1$college, FUN=length)
levels(df1$college)
levels(df0$college)

h0_or <- ggplot(data=df1, aes(x =factor(df1$'college'))) +geom_bar(stat = "count")
# + scale_y_continuous(breaks=seq(0,300,10)) + scale_x_continuous(breaks=seq(0,2090000,50000)) + theme(axis.text.x = element_text(angle=90, vjust=0.5)) + ggtitle("Nombre de marchÃ© selon \n leur montant et par expÃ©rimentateur, 17/08/2017")
# h0 + labs(fill= "Type local", x="Collège", y = "nombre") + geom_text(stat='count', aes(label=..count..), vjust=1.5)
h0_or + labs(fill= "Type local", x="Collège", y = "nombre") + geom_text(color = "white", stat='count', aes(label=..count..), vjust=1.5)

df1$college <- factor(df1$college, levels = df1$college[order(-df1$..count..)])

#h1 <- ggplot(data=df1, aes(x = reorder(df1$'college',-count(df1$'college')[2]))) +geom_bar(stat = "count")
# h1

h0 <- ggplot(data=df0, aes(x = fct_infreq(as.factor(df0$college)))) +geom_bar()
h0 + labs(fill= "Type local", x="Collège", y = "nombre") + geom_text(color = "white", stat='count', aes(label=..count..), vjust=1.5)


# https://stackoverflow.com/questions/37527053/order-multiple-geom-bar-in-ggplot2-bargraph


# https://stackoverflow.com/questions/42240820/sorting-bars-in-a-bar-chart-with-ggplot2
# https://stackoverflow.com/questions/26553526/how-to-add-frequency-count-labels-to-the-bars-in-a-bar-graph-using-ggplot2
# aes(fill=factor(df0$'CodeDepartement'))








# ==== density stacked ====
# https://stackoverflow.com/questions/12980081/create-a-stacked-density-graph-in-ggplot2



# =====  00 pie ====
# dfp <- as.data.frame(tapply(df0$'collège', df0$'CodeDepartement' , length), col.names=c('dep','nombre'))
# dfp <- as.data.frame(table(df0$'collège', df0$'CodeDepartement'))
dfp <- as.data.frame(table(df0$'CodeDepartement'))
dfp <- subset(dfp, dfp$Freq != 0)
# https://stackoverflow.com/questions/47752037/pie-chart-with-ggplot2-with-specific-order-and-percentage-annotations
# changement ordre
dfp$Var1 <- factor(dfp$Var1, levels = rev(as.character(dfp$Var1)))

# https://stackoverflow.com/questions/18209061/how-to-calculate-percentage-from-a-vector-of-counts
dfp %>% mutate((percent = Freq / sum(Freq)))
dfp$percent = round((dfp$Freq / sum(dfp$Freq)) * 100 , 2)

h0 <- ggplot(data=dfp, aes(x="", y = Freq, fill= factor(Var1))) +geom_bar(width=1, stat="identity")
# https://stackoverflow.com/questions/28912059/labels-on-the-pie-chart-for-small-pieces-ggplot
h0 +labs(fill="") + coord_polar("y", start=0) + labs(fill= "Dép", x="Collège", y = "nombre") + geom_text(color = "white", aes(label=paste(Freq, paste(percent, '%', sep=' '), sep='\n') , y=Freq / 2 + c(0, cumsum(Freq)[-length(Freq)]))) + theme(legend.position = "bottom",   ### Solution to part 1, no legend
        axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        axis.text  = element_blank())

## Graphique circulaire dép
# ```{r pie_dep, echo=FALSE, warning=FALSE}
bil_cot <- summaryBy(cotisation_annuelle  ~ nature_ad , data = df0, FUN=c(sum,mean,length), var.names=c('nature','cotis_som','cotis_mean', 'cotis_nb'))
bil_cot2 <- summaryBy(cotisation_annuelle  ~ df0$collège , data = df0, FUN=c(sum,mean,length), var.names=c('collège','cotis_som','cotis_mean', 'cotis_nb'))

dfp <- subset(dfp, dfp$Freq != 0)
# https://stackoverflow.com/questions/47752037/pie-chart-with-ggplot2-with-specific-order-and-percentage-annotations
# changement ordre
bil_cot2$cotisation_annuelle.sum <- factor(bil_cot2$cotisation_annuelle.sum, levels = rev(as.character(bil_cot2$cotisation_annuelle.sum)))
# https://stackoverflow.com/questions/18209061/how-to-calculate-percentage-from-a-vector-of-counts
bil_cot2$percent = round((bil_cot2[,2] / sum(bil_cot2[,2])) * 100 , 0)
p0 <- ggplot(data=bil_cot2, aes(x="", y = cotisation_annuelle.sum, fill= factor(collège))) +geom_bar(width=1, stat="identity")
# https://stackoverflow.com/questions/28912059/labels-on-the-pie-chart-for-small-pieces-ggplot
p0 +labs(fill="") + coord_polar("y", start=0, direction =-1) + labs(fill= "Dép", x="", y = "") + geom_text(color = "white", aes(label=paste(cotisation_annuelle.sum, paste(percent, '%', sep=' '), sep='\n') , y= (cotisation_annuelle.sum)/2 + c(0, cumsum(cotisation_annuelle.sum)[-length(cotisation_annuelle.sum)]))) +
  theme(legend.position = "right")   ### Solution to part 1, no legend





# ==== 70 treemap =====
# https://github.com/wilkox/treemapify

# install.packages("treemapify")
library("treemapify")

# ggplot(G20, aes(area = gdp_mil_usd, fill = hdi)) +geom_treemap()

# ggplot(df0, aes(fill=df0$cotisation_annuelle, area=df0$NbreHabitants)) +geom_treemap()
ggplot(df_ad, aes(fill=df_ad$cotisation_annuelle, area=df_ad$NbreHabitants, label= df_ad$libelle)) +geom_treemap() + 
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE)

# tree plot recapitulant la nature
# ==== recap ====
library(doBy)
recap <- summaryBy(cotisation_annuelle + NbreHabitants ~ nature_ad , data = df_ad, FUN=c(sum,mean,length))
# produces mpg.m wt.m mpg.s wt.s for each
# combination of the levels of cyl and vs 

ggplot(recap, aes(fill=recap$cotisation_annuelle.sum, area=recap$NbreHabitants.sum, label= recap$nature_ad)) +geom_treemap() + 
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre", grow = TRUE) + theme(legend.position = "bottom") +
  scale_fill_gradient(low="white",high="red") + labs(fill= "Montant Cotisation")
# TO DO faire attentiio dans la colorimétrie 


ggplot(recap, aes(fill=recap$NbreHabitants.sum, area=recap$cotisation_annuelle.sum, label= recap$nature_ad)) +geom_treemap() + 
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre", grow = TRUE) + theme(legend.position = "bottom") +
  scale_fill_gradient(low="white",high="red") + labs(fill= "Montant Cotisation")


## Treemap
# ```{r figdim2, fig.height=3, fig.width=5, fig.align='right', message=FALSE,} exemple pour deplacer les figures
df_ad <- subset(df0, df0$'college' != 1)
cat <- sapply(df_ad, is.factor)
recap <- summaryBy(cotisation_annuelle + NbreHabitants ~ nature_ad , data = df_ad, FUN=c(sum,mean,length))

ggplot(recap, aes(fill=recap$cotisation_annuelle.sum, area=recap$NbreHabitants.sum, label= recap$nature_ad)) +geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) + theme(legend.position = "bottom") 
# + scale_fill_conti(palette = "Spectral")




# ==== TO DO : frise de cotisation avec colorisaiton par group / college
# https://stackoverflow.com/questions/46027258/r-graph-label-by-group

# ==== graphe avec nouveau typo
h3 <- ggplot(data=df0, aes(x = factor(df0$'nature_ad'), fill=factor(df0$CodeDepartement))) +geom_bar() 
# + facet_wrap( ~ df0$'CodeDepartement')
h4 <- h3 + theme(legend.position = c(0.8, 0.7), legend.box = "horizontal") + labs(fill= "Département", x="Nature", y = "nombre")
h5 <- h4 + guides(col = guide_legend(nrow = 2)) + geom_text(size = 2.5, color = "white", stat='count', aes(label=..count..), position = position_stack(vjust = 0.5))
h5 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
+ scale_x_discrete("Cut", labels = c("Fair" = "F","Good" = "G",
                                     "Very Good" = "VG","Perfect" = "P","Ideal" = "I"))
h5 + scale_x_discrete(labels = abbreviate)
# https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2

leg.txt <- levels(factor(df0$nature_ad))
x.labels <- structure(LETTERS[seq_along(leg.txt)], 
                      .Names = leg.txt)
abbreviate(leg.txt)
monlabel <- structure(abbreviate(levels(factor(df0$nature_ad))))


# https://www.rdocumentation.org/packages/ggplot2/versions/2.2.1/topics/scale_x_discrete

h3 <- ggplot(data=df0, aes(x = factor(df0$'nature_ad'), fill=factor(df0$CodeDepartement))) +geom_bar() 
# + facet_wrap( ~ df0$'CodeDepartement')
h4 <- h3 + theme(legend.position = c(0.8, 0.7), legend.box = "horizontal") + labs(fill= "Département", x="Nature", y = "nombre")
h5 <- h4 + guides(col = guide_legend(nrow = 2)) + geom_text(size = 2.5, color = "white", stat='count', aes(label=..count..), position = position_stack(vjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

h5 + scale_x_discrete(labels = abbreviate)




## Distribution Cotisations Adhérents
```{r densite3, echo=FALSE}
p<-ggplot(df_ad, aes(x=df_ad$cotisation_annuelle, fill=factor(df_ad$CodeDepartement))) +
  geom_density(position = "stack", adjust = 1.5, size=1)
  p1 <- p + theme(legend.position = c(0.8, 0.8))  + labs(fill= "Département", x="Montant", y = "nombre")
p1 + scale_color_manual(values=c("red", "darkgreen", "gray"), name="légende")


# 
# ggplot(df_ad, aes(x=df_ad$cotisation_annuelle, ..count.., fill=factor(df_ad$CodeDepartement))) +
#   geom_density(position = "fill") + scale_colour_brewer(palette = "Set1")

#
# par comptage
ggplot(df_ad, aes(x=df_ad$cotisation_annuelle, ..count.., fill=factor(df_ad$nature_ad))) +
  geom_density(position = "fill") + scale_fill_brewer(palette = "Set1")




library(colorspace)
# 
# ggplot(df_ad, aes(x=df_ad$cotisation_annuelle, ..count.., fill=factor(df_ad$nature_ad))) +
#   geom_density(position = "fill")

unique(df0$'nature_ad')

# ===== point jitter ====
b2 <- ggplot(data=df_ad, aes(y=df_ad$'cotisation_annuelle', x=factor(df_ad$'CodeDepartement'))) +geom_point(alpha = 1/10)
b2 + labs(x="département", y = "montant de cotisation") +geom_jitter(alpha=0.1)


# === courbe de points selon nature TO DO Mettre un gradient
p3 <- ggplot(data=df_ad) + geom_point(aes(x=df_ad$cotisation_annuelle , y=df_ad$NbreHabitants , color=factor(df_ad$'nature_ad'))) + theme(legend.position = "right")
p3 + theme(legend.position = "right") + labs(color= "nature", x="Montant de Cotisation", y = "nombre d'habitants")

# https://stackoverflow.com/questions/16961921/plot-data-in-descending-order-as-appears-in-data-frame

# bar plot pour montrer le nombre d adherents
h1 <- ggplot(data=df0, aes(x = factor(df0$'collège'), fill=factor(df0$CodeDepartement))) +geom_bar()
# + facet_wrap( ~ df0$'CodeDepartement')
h2 <- h1 + theme(legend.position = c(0.8, 0.8), legend.box = "horizontal") + labs(fill= "Département", x="Collège", y = "nombre")
h2 + guides(col = guide_legend(nrow = 2))
# h2 + guides(col = guide_legend(nrow = 2)) + geom_text(color = "white", stat='count', aes(label=..count..), vjust=0, postion='stack')
h2 + guides(col = guide_legend(nrow = 2)) + geom_text(size = 2, color = "white", stat='count', aes(label=..count..), position = position_stack(vjust = 0.5))

head(df0)

# ===== distribution
ggplot(df0, aes(x=df0$'cotisation_annuelle')) + geom_density()

h2 <- ggplot(data=df0) +geom_histogram(data=df0, aes(df0$'cotisation_annuelle'), binwidth = 100)
h2b <- ggplot(data=df_ad_bg) +geom_histogram(data=df_ad_bg, aes(df_ad_bg$'cotisation_annuelle'), bins=20)
h2b

mean(df_ad$cotisation_annuelle)

# on va faire une densite enrichie
# https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2
# https://stackoverflow.com/questions/37660694/add-legend-to-geom-vline
# https://stackoverflow.com/questions/12545322/add-vline-to-existing-plot-and-have-it-appear-in-ggplot2-legend

# http://www.sthda.com/french/wiki/ggplot2-courbe-de-distribution-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
p<-ggplot(df_ad, aes(x=df_ad$cotisation_annuelle)) +
  geom_density(adjust = 1.5, size=1)+
  geom_vline(data=df_ad, aes(xintercept=mean(df_ad$cotisation_annuelle), colour="moyenne 1367 ???"),
             linetype="dashed", size=1.25)+
  geom_vline(data=df_ad, aes(xintercept=median(df_ad$cotisation_annuelle), colour="medianne 325 ???"),
             linetype="dashed", size=1.25)
p1 <- p + theme(legend.position = c(0.8, 0.8))  + labs(fill= "Département", x="Montant", y = "nombre")
p1 + scale_color_manual(values=c("red", "darkgreen", "gray"), name="légende")


# h2c <- ggplot(data=df_ad) +geom_density(data=df_ad, aes(df_ad$'cotisation_annuelle'), adjust = 1.5, size=1)
# h2d <- h2c + geom_vline(data=df_ad, aes(xintercept = mean(df_ad$cotisation_annuelle)), colour="red", linetype=2 , size=1.25, show.legend=T)
# h2e <- h2d + geom_vline(data=df_ad, aes(xintercept = median(df_ad$cotisation_annuelle)), colour="darkgreen", linetype=2 , size=1.25, show.legend=T)
# h2e + scale_linetype_identity()
# h2e + scale_linetype_manual()
# 
# 
# h2c + scale_linetype_manual(name="groups",
#                           values = c(df_ad$cotisation_annuelle="solid", mean(df_ad$cotisation_annuelle)="dashed"))
# h2e <- h2d + geom_vline(xintercept = median(df_ad$cotisation_annuelle), colour="darkgreen", linetype=2 , size=1.25) + theme(legend.position = "bottom") + labs(x="Montant de Cotisation", y = "densité")
# # https://stackoverflow.com/questions/38956647/how-to-add-custom-legend-with-ggplots-and-maps
# h2e + scale_linetype_manual(values = c('green', 'red', 'dark'), labels = c('Above Mean', 'At Mean', 'Below Mean'))

# deuxieme facon de rediger
h2c <- ggplot(data=df_ad, aes(df_ad$'cotisation_annuelle'), show.legend=TRUE) + geom_density(adjust = 1.5, size=1) 
h2d <- h2c +  geom_vline(aes(xintercept = mean(df_ad$cotisation_annuelle)), show.legend=TRUE, colour="red", linetype=2 , size=1.25)

h2d + theme(legend.position = c(0.8, 0.8), legend.box = "horizontal") + labs(fill= "Département", x="Collège", y = "nombre")
h2d +scale_color_manual("Line.Color", values=c(red="red",green="green",blue="blue"))



# https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
# https://stackoverflow.com/questions/17506053/making-line-legends-for-geom-density-in-ggplot2-in-r

# nouvelle façon
h2c <- ggplot(data=df_ad, aes(x=df_ad$'cotisation_annuelle')) +geom_density(data=df_ad, adjust = 1.5, size=1, show.legend=TRUE) + geom_vline(data=df_ad, aes(xintercept = mean(df_ad$cotisation_annuelle)), colour="red", linetype=2 , size=1.25, show.legend=FALSE)+ 
    stat_density(aes(x=df_ad$'cotisation_annuelle'), geom="line",position="identity")
h2c + theme(legend.position = "bottom") + labs(x="Montant de Cotisation", y = "densité")

h2c <- ggplot(data=df_ad) +geom_density(data=df_ad, aes(df_ad$'cotisation_annuelle'), adjust = 1.5, size=1) + labs(x="Montant de Cotisation", y = "densité")
h2c + geom_vline(xintercept = mean(df_ad$cotisation_annuelle,  show.legend=T), color="red", linetype=2 , size=1.25) + geom_vline(xintercept = median(df_ad$cotisation_annuelle), color="darkgreen", show.legend=T, linetype=2 , size=1.25)



h2d <- ggplot(data=df_no_mb, colour=df_ad$'CodeDepartement') +geom_density(data=df_no_mb, aes(df_no_mb$'cotisation_annuelle')) + coord_cartesian(xlim=c(0,5000))
h2d

h2e <- ggplot(data=df_ad) +geom_density(data=df_ad, aes(df_ad$'cotisation_annuelle', fill=factor(df_ad$'CodeDepartement'))) + coord_cartesian(xlim=c(0,1000))
h2e


tesob <- ggplot(data=df_ad) +geom_histogram(data=df_ad, aes(df_ad$'cotisation_annuelle', fill=factor(df_ad$'CodeDepartement')))
tesob

# on fait un agregat
# https://klein.uk/teaching/viz/datavis-pyramids/

teso1 <- ggplot(df_ad, aes(x= 'cotisation_annuelle')) + geom_bar(aes(x=df_ad$'cotisation_annuelle', fill=df_ad$'CodeDepartement'))
teso1

h2e <- ggplot(data = df_ad, aes(x = df_ad$'cotisation_annuelle')) + 
  geom_histogram(data= df_ad_bg , fill = 'grey', alpha = 0.5) + coord_cartesian(ylim=c(0,500), xlim(0, 30000)) +
  geom_histogram(colour="black") + coord_cartesian(ylim=c(0,500), xlim(0, 30000)) + facet_grid(factor(df_ad$'CodeDepartement'~.)) + 
  guides(fill = FALSE)   +theme_bw()# to remove the legend
  #   theme_bw()
h2e
# colour= "black"

h2f <- ggplot(data=df_ad_bg) + geom_histogram(df_ad_bg, aes(x=df_ad_bg$'cotisation_annuelle') , alpha=0.5)

# retente de code
h2e <- ggplot(data = df_ad, aes(x = df_ad$'cotisation_annuelle'), fill = df_ad$'CodeDepartement') + 
  geom_histogram(data= df_ad_bg , fill = 'grey', alpha = 0.5) + coord_cartesian(ylim=c(0,500), xlim(0, 30000)) +
  geom_histogram(colour="black") + coord_cartesian(ylim=c(0,500), xlim(0, 30000)) + facet_grid(factor(df_ad$'CodeDepartement'~.)) + 
  guides(fill = FALSE)   +theme_bw()# to remove the legend
#   theme_bw()
h2e

h2g <- ggplot(data = df_ad, aes(x = df_ad$'cotisation_annuelle', fill = df_ad$'CodeDepartement'))
h2g + geom_density(position="fill") 

# code exemple
d <- iris        # Full data set
d_bg <- d[, -5]  # Background Data - full without the 5th column (Species)

ggplot(d, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(data = d_bg, fill = "grey", alpha = .5) +
  geom_histogram(colour = "black") +
  facet_grid(~ Species) +
  guides(fill = FALSE) +  # to remove the legend
  theme_bw()


# variation des cotisations par département
h3 <- ggplot(data=df_ad) +geom_boxplot(data=df_ad, aes(y=df_ad$'cotisation_annuelle', x=factor(df_ad$'CodeDepartement')))
h3

# variation des cotisations par college pour les adherents
h4 <- ggplot(data=df_ad) +geom_boxplot(data=df_ad, aes(y=df_ad$'cotisation_annuelle', x=factor(df_ad$'collège')))
h4

# variation des cotisations par departement pour les adherents
h4c <- ggplot(data=df_ad) +geom_boxplot(data=df_ad, aes(y=df_ad$'cotisation_annuelle', x=factor(df_ad$'CodeDepartement')))
h4c
h4c + facet_wrap(.~factor(df_ad$'collège'))

# on combine
h4d <- ggplot(data=df_ad, aes(y=df_ad$'cotisation_annuelle', x=''))
h5d <- h4d + geom_boxplot(fill=factor(df_ad$'collège')) 
h5d +facet_wrap(.~factor(df_ad$'CodeDepartement'), scales='free')
 
h5 <- ggplot(data=df_ad_bg) + geom_boxplot(aes(y=df_ad_bg$'cotisation_annuelle', x=factor(df_ad_bg$'collège')))
h5 +facet_grid(factor(df_ad$'CodeDepartement')~., scales='free')
# h5 +facet_grid(factor(df_ad$'CodeDepartement')~., scales='free')


grid.arrange(h1, h3, h4, h5, ncol=2, nrow = 2)

h6 <- ggplot(data=df_no_mb) + geom_bar(aes(y=df_no_mb$'cotisation_annuelle', fill=factor(df_no_mb$'collège')))
h6


# Modele economique sous forme de points
p1 <- ggplot(data=df_ad) + geom_point(aes(x=df_ad$cotisation_annuelle , y=df_ad$NbreHabitants , color=factor(df_ad$'collège'))) + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Spectral")
p2 <- p1 + geom_smooth(aes(x=df_ad$cotisation_annuelle , y=df_ad$NbreHabitants), span = 0.15) 
p2 + theme(legend.position = "bottom") + labs(color= "collège", x="Montant de Cotisation", y = "nombre d'habitants")

# p2 + facet_wrap(~df_ad$CodeDepartement) 
# p1 + scale_color_gradientn(colours = rainbow(13))
# 
# p1 + scale_colour_gradient2()
# 
# length(unique(df_ad$collège)) # 13 couleurs


# ==== modele eco sous forme de point avec ellipse
# http://www.sthda.com/french/wiki/ggplot2-nuage-de-points-guide-de-d-marrage-rapide-logiciel-r-et-visualisation-de-donn-es
p1 <- ggplot(data=df_ad, aes(x=df_ad$cotisation_annuelle , y=df_ad$NbreHabitants , color=factor(df_ad$'collège'))) + geom_point() + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Spectral")
p1 + stat_ellipse(aes(x=df_ad$cotisation_annuelle , y=df_ad$NbreHabitants , color=factor(df_ad$'collège')), type="norm")
p1 + theme(legend.position = "bottom") + labs(color= "collège", x="Montant de Cotisation", y = "nombre d'habitants")



p1 + scale_fill_gradient2(ow = "red", hight = "blue", mid = "white", midpoint = 25) 
# p0 <- ggplot(data=nb7, aes(x='', y= montant_contrat.length, fill=categorie)) + geom_bar(width = 1, stat ='identity')
# p1 <- p0 + coord_polar("y", start = 0)
# p2 <- p1 + theme(legend.position = "right") + theme(panel.background = element_blank()) + labs(x='',y='') + ggtitle("Nombre de marchés \n selon leur catégorie") + theme(plot.title = element_text(hjust = 0.5))
# p3 <-p2 + theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank())
# # p4 <- p3 + geom_text(data=nb7, aes(x='', y = pos, label = paste0(round(pct,1),"%")), size=4)
# p4 <- p3 + geom_text(aes(label = round(pct)), size=3, position = position_stack(vjust = 0.5))  + coord_polar(theta = "y")
# # TO COMPLETE
+ geom_text(aes(y = 1/3, x= c(cumsum(montant_contrat.length) - montant_contrat.length/2), label = montant_contrat.length, size=5))
# TO COMPLETE

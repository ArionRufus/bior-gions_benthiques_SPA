graph\_rmark
================

-   [Initialisation](#initialisation)
-   [Analyses de complétude](#analyses-de-complétude)
    -   [Complétude par mont
        sous-marin](#complétude-par-mont-sous-marin)
    -   [Complétude par au sein de chaque mont
        sous-marin](#complétude-par-au-sein-de-chaque-mont-sous-marin)

Ce script est une peu un script à tout faire. Pour l’instant il réalise
2 taches:  
- Réprésentation et calculs de complétude,  
- Représentation des intervalles de profondeur des opérations de pêche.

# Initialisation

On va avoir besoin de plusieurs packages:  
- *tidyverse* pour la manipulation de données (tidyr, dplyr, …),  
- *ggplot2* pour la représentation graphique.

``` r
library(tidyverse)
library(ggplot2)

initialwd = ("C:/travail/analyses_spa/analyses_croixdusud")
setwd(initialwd)
```

<br/> <br/>

# Analyses de complétude

Le but de ces analyses est de savoir si l’effort d’échantillonage a été
assez important pour capturer l’essentiel de la diversité visée par cet
échantillonage.

Chargement des données:

``` r
catch_op_peche = readRDS("./modified_data/catch_op_peche.rds")
```

## Complétude par mont sous-marin

Par les analyses de complétude ciblées sur les monts sous-marins, on se
demande s’il aurait fallu échantillonner d’autres monts pour se rendre
compte de la diversité générale des monts sous-marins.  
Pour ça, on va représenter graphiquement l’évolution du nombre de
nouvelles espèces par mont sous-marin échantillonné. Ce graphique dépend
de l’ordre utilisé, pour le faire bien il faudrait peut-être soit
représenter tous les patterns différents, soit en faire une moyenne.
Ici, par manque de temps, on approxime en donnant seulement une des
représentations possibles, et en partant du principe que bien qu’elle
puisse varier, celle-ci est grossièrement représentative de la tendance
générale.  
Pour réaliser cette représentation, on a tout d’abord besoin du jeu de
données aggrégé par mont sous-marin, avec les différentes espèces pour
chaque mont. Il nous faut aussi la liste des monts:

``` r
#liste des monts sous-marin (rangés par ordre alphabéique, pour aider à la rpz graphique):
areas = unique(catch_op_peche$area)
areas = areas[order(areas)]

#fusion de ops de peche:
catch_area = catch_op_peche %>%
  dplyr::select(espece, nb, area) %>%
  group_by(area,espece) %>% 
  summarise(nbtot=sum(nb))

catch_area$espece = as.character(catch_area$espece) #pour pas avoir de soucis quand on fusionne les vecteurs
```

Ensuite on va partir d’un mont sous-marin, calculer sa diversité
spécifique et stocker dans un vecteur toutes ces espèces recensées.
Puis, pour chaque nouveau mont, on calcule le nombre d’espèces qui ne
sont pas déjà présentes dans le vecteur stock, on note cette nouvelle
diversité, et on stock ces nouvelles espèces, et ainsi de suite.  
On obtient alors un tableau avec pour chaque mont sous-marin le nombre
de nouvelles espèces recensées:

``` r
#le tableau qui stockera la diversite cumulee:
diver_sites = data.frame(
  monts = areas[1],
  div_cum = length(unique(which(catch_area$area == areas[1])))
)

#le vecteur qui stockera les especes deja comptees:
oldsps = unique(catch_area$espece[which(catch_area$area == areas[1])])

for (i in areas[-1]) { #pour chaque mont,
  #on trouve les nouvelles sps:
  newsps = catch_area$espece [which(catch_area$area == i)]
  newsps = newsps[-which(newsps %in% oldsps)]
  #on les range:
  oldsps = c(oldsps,newsps)
  #on range leur nombre dans le tableau:
  diver_sites =rbind(diver_sites,
                  data.frame(
                    monts  = i,
                    div_cum = length(oldsps)))
}
diver_sites
```

    ##               monts div_cum
    ## 1         16.Milles      14
    ## 2         45.milles      15
    ## 3         Amsterdam      18
    ## 4          Cap.Horn      20
    ## 5          Jean.Luc      22
    ## 6  Nordet.Amsterdam      25
    ## 7   Nordet.StPaul.1      26
    ## 8            StPaul      26
    ## 9              Tmp4      27
    ## 10             Tmp5      27

A la fin on a bien 27 espèces, soit le nb total d’espèces présentes dans
le jeu de données:

``` r
length(unique(catch_area$espece)) == diver_sites[nrow(diver_sites),2]
```

    ## [1] TRUE

On représente graphiquement ce jeu de données (c’est un lolopop plot:
simplement des point et un sègment):

``` r
ggplot(diver_sites, aes(x=monts, y=div_cum)) +
  geom_segment( aes(x=monts, xend=monts, y=0, yend=div_cum), color="grey") +
  geom_point( color="orange", size=4) +
  ylim(0, 30) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("sites") +
  ylab("Diversité cumulée")
```

![](graph_rmark_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> Une
stabilité globale semble grossièrement atteinte.

## Complétude par au sein de chaque mont sous-marin

Le principe est le même que la représentation précédente, pour chaque
mont sous-marin. On va donc réaliser le même code dans une boucle pour
chaque mont, et à la fin de chaque itération, assigner les résultats à
un tableau nomé en fonction du mont de l’itération:

``` r
for (x in areas) {
  #selection d'un site
  site = x
  
  #extraction des données du site:
  data_site = catch_op_peche[which(catch_op_peche$area == x),]
  nums_peche = unique(data.frame(
    data_site$mean.lat,
    data_site$mean.lon
  ))
  
  #le vecteur qui stockera les especes deja comptees:
  oldsps = data_site$espece[which(data_site$mean.lon == nums_peche$data_site.mean.lon[1] & 
                                  data_site$mean.lat == nums_peche$data_site.mean.lat[1])]
  
  #le tableau qui stockera la diversite cumulee:
  stock = data.frame(
    op = 1,
    div_cum = length(oldsps)
  )
  
  if(nrow(nums_peche)>1){ #si il y a plus d'une op de peche,
    for (i in 2:nrow(nums_peche)) { #pour chaque mont sauf le premier, deja compte,
      #on trouve les nouvelles sps:
      newsps = data_site$espece [which(data_site$mean.lon == nums_peche$data_site.mean.lon[i] & 
                                       data_site$mean.lat == nums_peche$data_site.mean.lat[i])]
      newsps = newsps[-which(newsps %in% oldsps)]
      #on les range:
      oldsps = c(oldsps,newsps)
      #on range leur nombre dans le tableau:
      stock =rbind(stock,
                   data.frame(
                     op  = i,
                     div_cum = length(oldsps)))
    }
  }
  stock$op = as.character(stock$op)
  assign(paste0(text = "diver_intra_", site) , stock)
}
```

Le plot suivant peut être réalisé pour chacun des tableaux créés:

``` r
ggplot(diver_intra_Nordet.StPaul.1, aes(x=op, y=div_cum)) +
  geom_segment( aes(x=op, xend=op, y=0, yend=div_cum), color="grey") +
  geom_point( color="orange", size=4) +
  ylim(0, 21) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("opérations de pêche") +
  ylab("Diversité cumulée")
```

![](graph_rmark_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

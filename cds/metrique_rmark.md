metrique\_rmark
================

-   [Initialisation](#initialisation)

# Initialisation

Ce script est sensé être une fonction, et donc commencer par déclarer
les variables dont il a besoin, puis réaliser les calculs à partir de
ces variables. Cependant, une fonction doit s’écrire en une fois, et ne
convient donc pas à ce format Rmd. Je vais donc extraire le code de la
fonction, le code initial contenant la fonction étant disponible dans le
github (accès direct via ce lien).  
Je vais donc commencer par déclarer les variables qu’on devrait donner à
notre fonction, à savoir:  
- *db*, la base de données sur laquelle ont été réalisées les
analyses.  
- *site.field*, la colonne des sites dans *db*.  
- *species.field*, la colonne des espèces dans *db*.  
- *species.richness*, la colonne de richesse de l’espèce dans *db*. Elle
est obtionelle, puisqu’on peut faire des analyses en présence-absence.  
- *network*, le tableau issu de la fonction *readInfomapTree* utilisé
pendant l’analyse de réseau, qui présente les résultats de cette
analyse.  
- *level*, le level de clustering du tableau *network* que l’on souhaite
étudier.

``` r
initialwd = ("C:/travail/analyses_spa/analyses_croixdusud/scripts/reseaux")
setwd(initialwd)

db = readRDS("C:/travail/analyses_spa/analyses_croixdusud/modified_data/catch_cluster_op.rds")
site.field = "num"
species.field = "espece"
species.richness = "cpue"
network = readRDS("C:/travail/analyses_spa/analyses_croixdusud/modified_data/catch_op.clusters.rds")
level = "lvl1" 
```

<br/>

Maintenant on crée le tableau qui va stocker tous les résultats.  
Il présente, pour chaque cluster, le nombre de sites: Pour chaque
cluster, calcule le nombre de sites différents qui sont dans la base de
donnée initiale et aussi inscrits dans *network*, associés au cluster
qu’on regarde. On ne peut pas directement calculer le nombre de sites
présents dans *network* car les sites ne sont pas différenciés des
espèces:

``` r
head(network)
```

    ##   Groups Codelength                  Name id lvl1                  lvl2
    ## 1    1:1  0.1594480             Mora moro 16    1             Mora moro
    ## 2    1:2  0.0314706                Tmp526 60    1                Tmp526
    ## 3    1:3  0.0311262 Etmopterus granulosus 22    1 Etmopterus granulosus
    ## 4    1:4  0.0309918                Tmp525 59    1                Tmp525
    ## 5    1:5  0.0257411                Tmp427 57    1                Tmp427
    ## 6    1:6  0.0206595     Nordet.StPaul.122 46    1     Nordet.StPaul.122
    ##   colors.lvl1
    ## 1     #A6CEE3
    ## 2     #A6CEE3
    ## 3     #A6CEE3
    ## 4     #A6CEE3
    ## 5     #A6CEE3
    ## 6     #A6CEE3

On ne peut donc pas visualiser quels sont les sites. Si on prend en
compte la richesse dans nos analyses, alors on greffe une 3ème colonne
au tableau, indiquant la richesse présente dans le cluster = présente
dans chacun des sites du cluster.

``` r
region.stats <- data.frame(
                    #les regions, = les clusters créés:
                    region = levels(network[,level]),
                    #nb.sites = combien on a de sites dans chaque cluster:
                    nb.sites = sapply(
                                levels(network[, level]), #pour chaque cluster,
                                function(x, net, database) {
                                  length(unique(database[which(database[, site.field] %in% 
                                  net$Name[which(net[, level] == x)]), site.field]))
                                }, net = network, database = db)
                    )
#si on prend en compte la richesse:
if (!is.null(species.richness)) {
  region.stats = cbind(region.stats,
                       richesse = sapply(
                         levels(network[, level]),
                         function(x, net, database) {
                           sum(
                             database[which(database[,site.field] %in% net$Name[which(net[, level] == x)]), 
                                      species.richness])
                         }, net = network, database = db))}

region.stats
```

    ##   region nb.sites  richesse
    ## 1      1       12 1942.0456
    ## 3      3        6  657.3773
    ## 4      4        6  423.7302
    ## 2      2        9 1264.0476

<br/>

La prochaine étape donne les métriques pour chaque espèce.  
Pour ce faire, un tableau est créé, donnant pour chaque espèce:  
- Le cluster d’appartenance,  
- *Ri*, le nombre de sites où l’espèce est présente au sein de son
cluster,  
- *Di*, le nombre total de sites où l’espèce est présente,  
- *Ai*, l’affinité: *Di* / nb de sites dans le cluster (info issue de
*region.stats*),  
- *Fi*, la fidélité: *Ri* / *Di*,  
- *IndVal*, l’indicative value: *Ai* \* *Fi* (a quel point l’espèce
participe à l’existance du cluster),  
- *DiVal*, l’indice de dilution: *Ai* \* (1-*Fi*) (a quel point l’espèce
crée des liens entre ce cluster et le reste du réseau).

Si un indice de richesse a été donné à la fonction, ces métriques vont
aussi être calculées en terme de richesse: - *Ri* = la richesse de
l’espèce dans les sites de son cluster,  
- *Di* = la richesse totale de l’espèce, tous sites compris,  
- *Ai*, l’affinité: *Di* / richesse du cluster (info issue de
*region.stats*).  
Les métriques suiventes sont calculées de la même manière qu’en présence
/ absence.

``` r
#On crée d'abord le tableau avec les différentes espèces:
sp.stats <- data.frame(
  species = levels(as.factor(db[, species.field])))
rownames(sp.stats) <- sp.stats$species

#Puis on le remplit:
for (sp in sp.stats$species) { #pour chaque espèce,
  #colonne cluster:
  #a quel cluster elle appartient
  sp.stats[sp, "cluster"] <- network[which(network$Name == sp), level]
  
  #quels sont les sites où elle est présente:
  site.occurrence.total <- unique(db[which(db[, species.field] == sp), site.field])
  
  #quels sont les sites du cluster ou elle est presente:
  site.occurrence.region <- site.occurrence.total[
    which(site.occurrence.total %in% network$Name[which(network[, level] == sp.stats[sp, "cluster"])])]
  
  #nb d'occurences dans le cluster:
  sp.stats[sp, "Occ.Ri"] <- length(site.occurrence.region)
  
  #nb d'occurences total:
  sp.stats[sp, "Occ.Di"] <- length(site.occurrence.total)
  
  #affinité: proportion nb de sites occupés vs nb de sites au sein du cluster:
  sp.stats[sp, "Occ.Ai"] <- sp.stats[sp, "Occ.Ri"] / region.stats$nb.sites[which(region.stats$region == 
                                                                                 sp.stats[sp, "cluster"])]
  
  #fidélité: proportion de sites occupé au sein du cluster vs dans tous les sites:
  sp.stats[sp, "Occ.Fi"] <- sp.stats[sp, "Occ.Ri"] / sp.stats[sp, "Occ.Di"]
  
  #metrique inval: a quel point l'sp participe au cluster:
  sp.stats[sp, "Occ.IndVal"] <- sp.stats[sp, "Occ.Ai"] *  sp.stats[sp, "Occ.Fi"]
  
  #metrique de dilution: a quel point l'sp est présente partout:
  sp.stats[sp, "Occ.DilVal"] <- sp.stats[sp, "Occ.Ai"] * (1 - sp.stats[sp, "Occ.Fi"])
  
  
  if (!is.null(species.richness)) {
    #richesse dans le cluster:
    #richesse cumulée
    sp.stats[sp, "Rich.Ri"] <- sum(
      db[which(db[,species.field] == sp & db[,site.field] %in% site.occurrence.region), 
         species.richness])
    
    #richesse total amenée par l'espèce:
    sp.stats[sp, "Rich.Di"] <- sum(db[which(db[,species.field] == sp), species.richness])
    
    #affinité: proportion de richesse amenée dans le cluster vs richesse dans le cluster:
    sp.stats[sp, "Rich.Ai"] <- sp.stats[sp, "Rich.Ri"] / 
      region.stats$richesse[which(region.stats$region == sp.stats[sp, "cluster"])]
    
    #fidélité: proportion de richesse amenée au sein du cluster vs dans tous les sites:
    sp.stats[sp, "Rich.Fi"] <- sp.stats[sp, "Rich.Ri"] / sp.stats[sp, "Rich.Di"]
    
    #metrique inval: a quel point l'sp participe au cluster:
    sp.stats[sp, "Rich.IndVal"] <- sp.stats[sp, "Rich.Ai"] *  sp.stats[sp, "Rich.Fi"]
    
    #metrique de dilution: a quel point l'sp est présente partout:
    sp.stats[sp, "Rich.DilVal"] <- sp.stats[sp, "Rich.Ai"] * (1 - sp.stats[sp, "Rich.Fi"])
  }
}
head(sp.stats)
```

    ##                               species cluster Occ.Ri Occ.Di     Occ.Ai
    ## Antimora rostrata   Antimora rostrata       1      1      1 0.08333333
    ## Bassanago nielseni Bassanago nielseni       3      6     24 1.00000000
    ## Beryx decadactylus Beryx decadactylus       3      4     11 0.66666667
    ## Beryx splendens       Beryx splendens       1      3      7 0.25000000
    ## Hydrolagus sp2         Hydrolagus sp2       1      2      2 0.16666667
    ## Hydrolagus sp1         Hydrolagus sp1       1      2      3 0.16666667
    ##                       Occ.Fi Occ.IndVal Occ.DilVal    Rich.Ri    Rich.Di
    ## Antimora rostrata  1.0000000 0.08333333 0.00000000  0.7936508  0.7936508
    ## Bassanago nielseni 0.2500000 0.25000000 0.75000000 20.3406593 41.3801807
    ## Beryx decadactylus 0.3636364 0.24242424 0.42424242 13.8974359 19.6271112
    ## Beryx splendens    0.4285714 0.10714286 0.14285714  7.1710579  9.0648308
    ## Hydrolagus sp2     1.0000000 0.16666667 0.00000000  0.9523810  0.9523810
    ## Hydrolagus sp1     0.6666667 0.11111111 0.05555556  0.5555556  0.9365079
    ##                         Rich.Ai   Rich.Fi  Rich.IndVal  Rich.DilVal
    ## Antimora rostrata  0.0004086674 1.0000000 0.0004086674 0.0000000000
    ## Bassanago nielseni 0.0309421388 0.4915556 0.0152097814 0.0157323574
    ## Beryx decadactylus 0.0211407302 0.7080734 0.0149691893 0.0061715409
    ## Beryx splendens    0.0036925282 0.7910857 0.0029211062 0.0007714220
    ## Hydrolagus sp2     0.0004904009 1.0000000 0.0004904009 0.0000000000
    ## Hydrolagus sp1     0.0002860672 0.5932203 0.0001697009 0.0001163663

<br/>

On calcule ensuite les métriques pour les sites.  
Pour déterminer le participation d’un site au cluster, on regarde le
nombre d’espèces représentatives du cluster \* à quel point il est
classique de les croiser dans ce cluster, moins le nombre d’espèces non
représentatives \* à quel point il est anormal des les croiser dans ce
cluster. En termes mathématiques: Particip(site) = Somme pour chaque sp
représentative de son *IndVal* - somme pour chaque sp non représentative
de sa *DiVal*.  
Pour la richesse, on mulktiplie à chaque fois l’*IndVal* et la *DidVal*
par la richesse.  
Enfin, on calcule une participation pondérée pa le nombre d’espèces dans
le site, mais je ne comprends pas tellement l’intérêt, étant donné que
changer le nombrede sites sans changr la proportion de sites
représentatifs ou non ne changera pas les résultats.

``` r
#On crée d'abord le tableau avec les différents sites:
site.stats <- data.frame(site = levels(as.factor(db[, site.field])))
rownames(site.stats) <- site.stats$site

#puis on le remplit:
for (site in site.stats$site) {
  #cluster: a quel cluster appartient le site
  site.stats[site, "cluster"] <- network[which(network$Name == site), level]
  
  #les sp présentes au sein du site:
  sp.in.site <- unique(db[which(db[, site.field] == site), species.field])
  
  #les sps présentes qui appartiennent au cluster:
  characteristic.sp <- sp.in.site[which(sp.in.site %in% 
                                          network$Name[which(network[, level] == site.stats[site, 
                                                                                            "cluster"])])]
  #les sps présentes qui n'appartiennent pas au cluster:
  noncharacteristic.sp <- sp.in.site[which(!(sp.in.site %in% 
                                               network$Name[which(network[, level] == site.stats[site, 
                                                                                                 "cluster"])]))]
  #participation du site: Participation des sps caractéristiques - dilution des espèces non caractéristiques:
  site.stats[site, "Occ.Rg"] <- 
    sum(sp.stats$Occ.IndVal[which(sp.stats$species %in% characteristic.sp)]) - 
    sum(sp.stats$Occ.DilVal[which(sp.stats$species %in% noncharacteristic.sp)])
  
  #participation pondérée du site: participation / nb d'especes dans ce site:
  site.stats[site, "Occ.RRg"] <- site.stats[site, "Occ.Rg"] / length(sp.in.site)
  
  
  if (!is.null(species.richness)) {
    #participation du site: Participation des sps caractéristiques - dilution des espèces non caractéristiques:
   site.stats[site, "Rich.Rg"] <- 
        sum(sp.stats$Rich.IndVal[which(sp.stats$species %in% characteristic.sp)] * 
        sp.stats$Rich.Ri[which(sp.stats$species %in% characteristic.sp)]) - 
        sum(sp.stats$Rich.DilVal[which(sp.stats$species %in% noncharacteristic.sp)] *
        sp.stats$Rich.Ri[which(sp.stats$species %in% noncharacteristic.sp)])
    
    #participation pondérée du site: participation / nb d'especes dans ce site:
    site.stats[site, "Rich.RRg"] <- site.stats[site, "Rich.Rg"] / length(sp.in.site)
  }
}
head(site.stats)
```

    ##                    site cluster     Occ.Rg     Occ.RRg    Rich.Rg  Rich.RRg
    ## 16.Milles10 16.Milles10       4 -0.3121511 -0.04459301 -161.49120 -23.07017
    ## 16.Milles11 16.Milles11       4  0.5000000  0.12500000   60.35238  15.08810
    ## 16.Milles18 16.Milles18       4 -0.3121511 -0.04459301 -161.49120 -23.07017
    ## 16.Milles19 16.Milles19       1  0.6775746  0.07528607  631.18325  70.13147
    ## 16.Milles20 16.Milles20       4 -1.1805721 -0.16865316 -161.85573 -23.12225
    ## 45.milles13 45.milles13       1 -0.0225387 -0.00450774  842.16948 168.43390

<br/>

Enfin, les résultats sont présentés dans la fonction sous forme de
liste:

``` r
results = list(region.stats = region.stats, species.stats = sp.stats, site.stats = site.stats)
```

<br/>

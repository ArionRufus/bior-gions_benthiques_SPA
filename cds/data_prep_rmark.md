data\_preparation
================

-   [Initialisation](#initialisation)
-   [accquisition et transformation des
    données](#accquisition-et-transformation-des-données)
    -   [transformation de Catch](#transformation-de-catch)
    -   [transformation de op\_peche](#transformation-de-op_peche)
    -   [transformation de banc\_peche](#transformation-de-banc_peche)
-   [Fusion des données](#fusion-des-données)

Ce script part des données en csv issues du document excell de la
camapgne de pêche scientifique *Croix du sud*.  
Le but de ce script est de tranformer et fusionner ces données pour
arriver à un tableau (*catch\_op\_peche*), dans lequel chaque ligne
présente une espèce pêchée lors d’une opération de pêche. Les colonnes
présentent de nombreuses variables, soit relatives à l’opération de
pêche (position GPS, profondeur, identifiant de l’opération de pêche,
…), soit relative à l’espèce (nom de l’espèce, nombre d’individus
pêchés, …).  
Ce script est découpé en 3 parties (cf sommaire ci-dessus). D’abord
l’initialisation classique du script, puis la mise en forme des 3 jeux
de données, et enfin la fusion de ces 3 jeux de connées pour n’en avoir
qu’un à la fin. A chaque sous-partie de la mise en forme, une petite
intro précise le contenu du jeu de données, et les actions qui vont y
être faites.

Ce script est une modification du script de base réalisé par Aurélien
Favreau et Jules Selles, merci à eux pour leur travail qui m’a
grandement facilité la tache. <br/> <br/> <br/> <br/>

# Initialisation

On va avoir besoin de plusieurs packages:  
- *dplyr* et *tidyr* pour la manipulation de données,  
- *lubridate* pour formater l’écriture de la date / heure.  
- *sf* pour manipuler des données spatialisées.

``` r
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)

initialwd = ("C:/travail/analyses_spa/analyses_croixdusud")
setwd(initialwd)
```

<br/> <br/> <br/> <br/>

# accquisition et transformation des données

## transformation de Catch

Catch contient des informations sur les prises de chaque op de peche.  
transformations à faire :  
- Mettre les noms d’espèces en latin  
- Sommer les données

Lecture du fichier:

``` r
catch_peche <- read.csv("./original_data/bio/Catch.csv", sep=";", na.strings=c(""))
head(catch_peche)
```

    ##      type numero modèle.casier position              espece nb kg.brut kg.net
    ## 1 filière      1            CG        1 Beuroisia duhameli   1      NA     NA
    ## 2 filière      1            CG       17           Mora moro  1       2     NA
    ## 3 filière      1            CG       17   siki petite épine  1      30     NA
    ## 4 filière      1            CG        9           Mora moro  1       4     NA
    ## 5 filière      1            CG        9   siki petite épine  1      35     NA
    ## 6 filière      1            RO        2 Beuroisia duhameli   1      NA     NA

<br/>

Donner le bon nom aux espèces:  
Pour ça on exécute un script qui crée une liste nommée
*level\_key\_taxon*, donnant les noms scientifiques correspondants aux
différents noms communs.

``` r
source('./scripts/species_names.R')
head(level_key_taxon)
```

    ##                 Beuroisia duhameli               Chaecon (crabe orange) 
    ##                "Beuroisia duhameli"                 "Chaceon paulensis" 
    ##         requin grande épine (en D2) requin grande épine (D2) peau lisse 
    ##             "Etmopterus granulosus"             "Etmopterus granulosus" 
    ##           requin grande épine lisse             Siki grande épine en D2 
    ##             "Etmopterus granulosus"             "Etmopterus granulosus"

Ici, on a par exemple *requin grande épine (D2) peau lisse* -&gt;
*“Etmopterus granulosus”*. <br/>

``` r
catch_peche <- catch_peche %>% 
  mutate(espece = recode(factor(espece), !!!level_key_taxon)) %>% #renommer les espèces, en facteurs
  select(-c(modèle.casier, position)) #enlever les variables inutiles
```

<br/>

Au sein d’un même numéro de pêche, certaines espèces sont recensées
plusieurs fois. On somme ces données:

``` r
catch_peche <- catch_peche %>% group_by(type, numero, espece) %>%
  dplyr::summarize(nb=sum(nb,na.rm=T),
                   kg.brut=sum(kg.brut,na.rm=T),
                   kg.net=sum(kg.net,na.rm=T), )
```

<br/> <br/> <br/> <br/>

## transformation de op\_peche

Op\_peche nous donne des variables liées à chaque op de peche.  
Transformations à faire :  
- Corriger les erreurs sur les valeurs numériques.  
- Mettre en forme les variables temporelles et spatiales.  
- Moyenner les valeurs de profondeur et localisation entre le début et
la fin de la ligne de pêche.  
- Fusionner les valeurs par opération de pêche.

Lecture du fichier:

``` r
op_peche <- read.csv(file="./original_data/bio/OP.csv", sep = ";", na.strings=c(""))
head(op_peche)
```

    ##      TYPE numero  type n date_filage heure_debut_filage heure_fin_filage
    ## 1 filière      1 ancre 0  07/06/2001              20:40             <NA>
    ## 2 filière      1    CG 1  07/06/2001               <NA>             <NA>
    ## 3 filière      1    AU 2  07/06/2001               <NA>             <NA>
    ## 4 filière      1    JP 3  07/06/2001               <NA>             <NA>
    ## 5 filière      1    RO 4  07/06/2001               <NA>             <NA>
    ## 6 filière      1    CG 5  07/06/2001               <NA>             <NA>
    ##   sonde_filage lat_filage sud_filage long_filage  est_filage vts_filage
    ## 1          632         37   43.78000          77       49.78         NA
    ## 2          632         37   43.76000          77 49.73994418         NA
    ## 3          632         37   43.78875          77 49.66807616         NA
    ## 4          648         37   43.81750          77 49.59620767         NA
    ## 5          654         37   43.84625          77 49.52433872         NA
    ## 6          667         37   43.87500          77  49.4524693         NA
    ##   appât_filage obs_filage date_virage heure_virage sonde_virage lat_virage
    ## 1         <NA>       <NA>  08/06/2001        07:25           NA         37
    ## 2         <NA>       <NA>  08/06/2001         <NA>           NA         NA
    ## 3         <NA>       <NA>  08/06/2001         <NA>           NA         NA
    ## 4         <NA>       <NA>  08/06/2001         <NA>           NA         NA
    ## 5         <NA>       <NA>  08/06/2001         <NA>           NA         NA
    ## 6         <NA>       <NA>  08/06/2001         <NA>           NA         NA
    ##   sud_virage long_virage est_virage obs_virage H_files_virage H_vires_virage
    ## 1      43.83          77      49.05       <NA>             NA             NA
    ## 2         NA          NA         NA       <NA>             NA             NA
    ## 3         NA          NA         NA       <NA>             NA             NA
    ## 4         NA          NA         NA       <NA>             NA             NA
    ## 5         NA          NA         NA       <NA>             NA             NA
    ## 6         NA          NA         NA       <NA>             NA             NA

<br/>

### Correction des erreurs sur les valeurs numériques:

Lorsqu’on essaye de passer toutes les variables sensées être numériques
en numériques, on voit qu’il y a des erreurs (des NA sont créés):

``` r
#sélection des variables sensées être numériques:
initial_data = op_peche %>%
  dplyr :: select(starts_with(c("long","est","vts","sud","lat","sonde"))) 

#as numeric:
trans_data = initial_data %>% 
  mutate_all(.funs=function(.x) as.numeric(as.character(.x)))
```

    ## Warning in (function (.x) : NAs introduits lors de la conversion automatique

<br/>

On va extraire dans un tableau les valeurs et positions des erreurs.
Pour ce faire, on regarde quelles valeurs ont été transformées en Na
lors de la conversion des variables en numériques (et ne l’étaient pas
avant):

``` r
difs_tab = data.frame() #tableau qui stockera les erreurs
for (i in colnames(initial_data)) { #pour chacune des variables numériques,
  #récupérer la variable avant et après conversion numérique
  before = paste0("initial_data", "$", i)
  after = paste0("trans_data", "$",i)
  #trouver les NA après conversion qui le sont pas avant
  changes = which(
    is.na(eval(parse(text = after))) & 
    !is.na(eval(parse(text = before))))
  if (length(changes)>0) { #s'il y en a, les inclure au tableau
    difs_tab =rbind(difs_tab,
                    data.frame(
                      rep(i,length(changes)), #la variable
                      changes,#la position de la valeur
                      eval(parse(text = paste0(before,'[changes]'))))) #la valeur
  }
}
colnames(difs_tab) = c('col','position','value')
difs_tab
```

    ##          col position  value
    ## 1 est_filage       54  50,00
    ## 2 est_filage      458  24,00
    ## 3 est_filage      614  38,00
    ## 4 est_filage      832 30..58

<br/>

Il y a 4 valeurs avec des fautes de frappes, toutes issues de la colonne
*est\_filage*.  
On les change, et on supprime les jeux de données créés, qui sont
maintenant inutiles:

``` r
op_peche$est_filage[54] = 50.0
op_peche$est_filage[458] = 24.0
op_peche$est_filage[614] = 38.0
op_peche$est_filage[832] = 30.58

rm(initial_data, trans_data, before, after, changes, difs_tab) #on supprime les variables créées, maintenant inutiles
```

<br/>

On repasse les varaibles en numérique, on regarde s’il n’y plus
d’erreurs, et on met en forme les variables temporelles du jeu de
données:

``` r
op_peche <- op_peche %>%
  
  #on repasse les variables en numérique:
  mutate(across(starts_with(c("long","est","vts","sud","lat","sonde")),
         .fns=function(.x) as.numeric(as.character(.x)))) %>%
  
  #on remplace les dates / heures en NA par 0:
  mutate(across(starts_with(c("date","heure")), 
         .fns=function(.x) replace_na(.x, 0))) %>%
  
  #on fusionne date et heure en une variable:
  unite('date_heure_virage', c("date_virage", "heure_virage"), sep=" ", na.rm = T) %>% 
  unite('date_heure_filage', c("date_filage", "heure_debut_filage"), sep=" ", na.rm = T) %>% 
  #rewriting of date /hour in new columns:
  #(warning message is relative to 0s (former NAs) data)
  mutate(date_virage= parse_date_time( date_heure_virage, c("%d/%m/%y %H:%M", "%d/%m/%y")),
         date_filage= parse_date_time( date_heure_filage, c("%d/%m/%y %H:%M", "%d/%m/%y")))
```

    ## Warning: 493 failed to parse.

    ## Warning: 760 failed to parse.

<br/>

On a 2 données de profondeur et de localisation différentes pour chaque
op de peche: le début et la fin de l’op (filage et virage). On va en
extraire une moyenne.  
Pour la localisation, les données sont en degrés minutes. On les
transforme en décimales en additionnant aux degrés les minutes / 60, et
comme on lit des coordonnées absolues et qu’on est au Sud, on rend la
latitude negative:

``` r
op_peche <- op_peche %>% 
  rowwise() %>% 
  mutate(sonde=mean(c(sonde_filage,sonde_virage), na.rm=T),
         lat = -mean( c(lat_filage + (sud_filage/60),lat_virage + (sud_virage/60)),na.rm=T),
         lon =  mean( c(long_filage + (est_filage/60),long_virage + (est_virage/60)),na.rm=T))
```

<br/>

Pour chaque opération de peche, on a plusieurs donées le long des
palangres et des casiers. On crée un nouveau jeu de données aggrégé à
l’opératon de pêche (par la moyenne):

``` r
op_peche_agreg <- op_peche %>% group_by(TYPE, numero)%>% 
  dplyr::summarise(mean.sonde = mean(sonde,na.rm=T),
                   max.sonde = max(sonde,na.rm=T),
                   min.sonde = min(sonde,na.rm=T),
                   mean.lat = mean(lat,na.rm=T),
                   mean.lon = mean(lon,na.rm=T),
                   casier.virage= n(), #Variable donnant le nombre de casiers
                   H.virage = mean(H_vires_virage, na.rm=T),
                   time.filage = min(date_filage, na.rm=T),
                   time.virage = min(date_virage, na.rm=T)) %>% ungroup() %>%
  mutate(casier.virage= ifelse(TYPE == 'filière', casier.virage, NA))
head(op_peche_agreg)
```

    ## # A tibble: 6 x 11
    ##   TYPE    numero mean.sonde max.sonde min.sonde mean.lat mean.lon casier.virage
    ##   <chr>    <int>      <dbl>     <dbl>     <dbl>    <dbl>    <dbl>         <int>
    ## 1 filière      1       655.      690.      632     -37.7     77.8            19
    ## 2 filière      2       812.      842       783     -37.7     77.8            21
    ## 3 filière      3       758.      803       640     -37.7     77.8            14
    ## 4 filière      4       760.      938       722     -37.7     77.9            20
    ## 5 filière      5       330.      354       312.    -38.8     77.6            19
    ## 6 filière      6       373.      406       247     -38.8     77.6            23
    ## # ... with 3 more variables: H.virage <dbl>, time.filage <dttm>,
    ## #   time.virage <dttm>

<br/>

Pour pouvoir plus tard représenter graphiquement des isobaths, on ajoute
une variable de groupes bathymétriques. Pour ça, on utilise cut, qui
divise un vecteur en ranges:

``` r
op_peche_agreg <- op_peche_agreg %>% 
  mutate(sonde.range=cut(mean.sonde, seq(0,max(mean.sonde)+50,by=50))) %>%
  #we specify max(mean.sonde)+50 otherwise the max of seq is under the max specified
  relocate(sonde.range, .after = mean.sonde) %>% #put the new variable on the right of mean.sonde
  relocate(H.virage, .after = time.virage)
```

<br/> <br/> <br/> <br/>

## transformation de banc\_peche

banc\_peche contient les noms, et l’emplacement des sommets des bancs
(=monts sous-marins). transformations à faire :  
- mettre la localisation au format SF  
- obtenir le mont sous-marin le plus proche de chaque op de pêche

Lecture du fichier:

``` r
banc_peche <-  read.csv('./original_data/Banc_id.csv', sep=";", na.strings=c("_"))
head(banc_peche)
```

    ##        lon      lat              name         local_name international_name
    ## 1 78.01667 37.60000 banc temporaire 1 Nordet.Amsterdam.1 Boomerang Seamount
    ## 2 77.80000 37.71667 banc temporaire 2 Nordet.Amsterdam.2 Boomerang Seamount
    ## 3 77.88333 37.65000 banc temporaire 3 Nordet.Amsterdam.2 Boomerang Seamount
    ## 4 77.73333 38.00000 banc temporaire 4               Tmp4                   
    ## 5 77.71667 38.25000 banc temporaire 5               Tmp5                   
    ## 6 77.65000 38.55000 banc temporaire 6    Nordet.StPaul.1                   
    ##           source_name
    ## 1 Johnson et al. 2000
    ## 2 Johnson et al. 2000
    ## 3 Johnson et al. 2000
    ## 4                    
    ## 5                    
    ## 6

<br/>

On met les latitudes en absolu (donc en négatif), et on traduit la
localisation en format sf:  
(banc\_id servira a fusionner les monts sous-marins avec les autres jeuc
de données)

``` r
banc_peche <- banc_peche %>% 
  rowwise() %>% 
  mutate(lat = -lat) %>% #comme pour op peche, -lat parce qu'on est dans le sud
  ungroup() %>%
  mutate(banc.id = as.numeric(row.names(banc_peche))) %>%
  st_as_sf(., coords = c("lon","lat"))
head(banc_peche)
```

    ## Simple feature collection with 6 features and 5 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: 77.65 ymin: -38.55 xmax: 78.01667 ymax: -37.6
    ## CRS:            NA
    ## # A tibble: 6 x 6
    ##   name           local_name       international_name  source_name        banc.id
    ##   <chr>          <chr>            <chr>               <chr>                <dbl>
    ## 1 banc temporai~ Nordet.Amsterda~ "Boomerang Seamoun~ "Johnson et al. 2~       1
    ## 2 banc temporai~ Nordet.Amsterda~ "Boomerang Seamoun~ "Johnson et al. 2~       2
    ## 3 banc temporai~ Nordet.Amsterda~ "Boomerang Seamoun~ "Johnson et al. 2~       3
    ## 4 banc temporai~ Tmp4             ""                  ""                       4
    ## 5 banc temporai~ Tmp5             ""                  ""                       5
    ## 6 banc temporai~ Nordet.StPaul.1  ""                  ""                       6
    ## # ... with 1 more variable: geometry <POINT>

<br/>

Obtenir le banc le plus proche de chaque op de peche:  
Pour ca, on transforme les coordonnées des op\_peche en format sf, puis
on crée un tableau des distances entre les coordonnées des bancs et des
op\_peche, avant de choisir pour chaque op\_peche le numéro du banc
ayant la plus petite distance.

``` r
st_op_peche = op_peche_agreg %>%sf::st_as_sf(coords = c('mean.lon','mean.lat')) #coordinates in sf format

closest_banc = st_op_peche %>% rowwise()%>%
  st_distance(banc_peche) %>% #pour chaque op, determine la distance geographique a tous les bancs
  as.data.frame() %>%
  rowwise()%>%
  summarise(banc.id.min=which.min(c_across(1:nrow(banc_peche)))) #le distance minimale
closest_banc
```

    ## # A tibble: 68 x 1
    ##    banc.id.min
    ##          <int>
    ##  1           2
    ##  2           2
    ##  3           2
    ##  4           3
    ##  5          13
    ##  6          13
    ##  7          13
    ##  8          13
    ##  9          13
    ## 10          13
    ## # ... with 58 more rows

Le tableau représente en première colonne chaque op de peche, et en
deuxième l’ID du mont sous-marin le plus proche. <br/> <br/> <br/> <br/>
<br/>

# Fusion des données

-   Fusionner les données en un tableau  
-   calculer les CPUEs  
-   Enlever les données pélagiques et crustacés  
-   Fusioner les sites Nordet Amsterdam  
-   Exporter les données.

On fusionne op\_peche\_agreg + closest banc, et le nom des bancs, en les
associant pas l’ID des bancs. Puis on fusionne avec catch\_peche, par
numero d’opération de peche (et par type, ça ne change rien mais ca
fusionne les 2 colonnes):

``` r
op_peche_agreg = left_join(
  op_peche_agreg %>% mutate(banc.id = closest_banc$banc.id.min), #put closest_banc in op_peche_agreg
  banc_peche %>% dplyr::select('banc.id', 'name', 'local_name'), 
  by=c('banc.id'='banc.id')) %>% 
  rename(area=local_name) #%>%

catch_op_peche = left_join(catch_peche, op_peche_agreg, by=c( 'numero',"type"="TYPE"))
```

<br/>

On supprime les variables inutiles, Et on calcule les cpue en nombre
d’individus et biomasses pêchées par temps de peche (ou par nombre de
casiers récupérés, pour les peches au casier):

``` r
catch_op_peche <- catch_op_peche %>% group_by(espece, numero, type, area, mean.sonde) %>%
  select(-c(banc.id, name)) %>% #remove useless variables
  mutate(CPUE.hooks= 1000*(kg.brut/H.virage),
         CPUE.nb.hooks= 1000 *(nb/H.virage),
         CPUE.traps= kg.brut/ casier.virage, #for crustaceans
         CPUE.nb.traps= nb/ casier.virage #for crustaceans
  )
```

<br/>

Enlever si on veut les espèces pélagiques, les lignes verticales ciblant
les espèces pélagiques, et les peches au casier, ciblant les crustacés:
(J’ai pas réussi a savoir si les 2 noms communs sont pélagiques, je n’ai
pas trouvé d’infos dessus, mais ils sont similaires a des espèces de
requins pélagiques, et étaient enlevés dans le script initial)

``` r
catch_op_peche = catch_op_peche %>%
  dplyr::filter(! type %in% c('filière', 'ligne verticale')) %>% #enlever casiers et lignes verticales
  droplevels() %>%
  select(-c(casier.virage, CPUE.traps, CPUE.nb.traps)) %>% #enlever les variables liées taux casiers
  dplyr::filter(! espece %in% c("Isurus oxyrinchus", "Prionace glauca",  #enlever les especes pelagiques
                                "requin gris sombre a museau pointu",
                                "requin brun a museau court", 
                                "Thyrsites atun")) %>% 
  droplevels()
```

<br/>

Fusionner les differents bancs Nordet Amsterdam (Il y a plusieurs monts
sous-marins qui ont été identifiés et qui s’apparentent à un même gros
mont, Nordet Amsterdam) :

``` r
ams_place = which(catch_op_peche$area == 'Nordet.Amsterdam.1'| catch_op_peche$area == "Nordet.Amsterdam.2")
catch_op_peche$area[ams_place] = rep("Nordet.Amsterdam",length(ams_place))
catch_op_peche$area = as.character(catch_op_peche$area)
head(catch_op_peche)
```

    ## # A tibble: 6 x 19
    ## # Groups:   espece, numero, type, area, mean.sonde [6]
    ##   type    numero espece       nb kg.brut kg.net mean.sonde sonde.range max.sonde
    ##   <chr>    <int> <fct>     <int>   <dbl>  <dbl>      <dbl> <fct>           <dbl>
    ## 1 ligne ~      1 Bassanag~    28    15.9    0         692. (650,700]        796.
    ## 2 ligne ~      1 Hydrolag~     1     5      0         692. (650,700]        796.
    ## 3 ligne ~      1 Helicole~    87    85.3   24.4       692. (650,700]        796.
    ## 4 ligne ~      1 Hyperogl~     4    47.7   25.9       692. (650,700]        796.
    ## 5 ligne ~      1 Mora moro    22    37.1   14.9       692. (650,700]        796.
    ## 6 ligne ~      1 Pseudotr~     1    15      0         692. (650,700]        796.
    ## # ... with 10 more variables: min.sonde <dbl>, mean.lat <dbl>, mean.lon <dbl>,
    ## #   time.filage <dttm>, time.virage <dttm>, H.virage <dbl>, area <chr>,
    ## #   geometry <POINT>, CPUE.hooks <dbl>, CPUE.nb.hooks <dbl>

<br/>

exporter le jeu de données final:

``` r
saveRDS(catch_op_peche, file='./modified_data/catch_op_peche.rds')
```

Readme
================

Dans ce Github je présente les analyses que j’ai réalisées dans le but
de créer des cartographies prédictives des biorégions de poissons
démersaux et d’invertébrés benthiques autour des îles de Saint-Paul et
Amsterdam. Il se présente en différents scripts expliqués en détails en
format *Rmarkdown* via les liens qui suivent. Les données initiales ne
sont pas pubiques, mais sont disponibles sur demande.  
<br/> <br/> <br/>

## Scripts à paritr des données Croix\_du\_Sud (poissons démersaux):

[Préparation des
données](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/cds/data_prep_rmark.md).
C’est une mise en forme des données, et correction des erreurs qui y
sont présentes, qui aboutit au tableau *catch\_op\_peche* que l’on
utilisera pour les analyses. Ce script est une adaptation d’un script
initial réalisé par Aurélien Favreau et Jules Selles.  
<br/> [Analyses de
réseaux](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/cds/reseaux_rmark.md).
Ce script part du tableau *catch\_op\_peche* pour réaliser les analyses
de réseau. En sortie, il crée un tableau présentant les clusters
déterminés pour chaque opération de pêche, *catch\_res*, ainsi que des
fichiers permettant de représenter geaphiquement les analyses de réseau
réalisées, fichiers disponibles
[ici](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/cds/fichiers_Gephi).
Ce script utilise une fonction calculant les métriques qui a été
modifiée, [la
voici](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/cds/metrique_reseaux.R).  
<br/> [Analyses de
corrélation](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/cds/correl_rmark.md).
Ce script corrèle les clusters trouvés a des données environnementales,
et à partir de ces corrélations, prédit la répartition des clusters sur
la zone de Saint-Paul / Amsterdam.  
<br/>
[Cartographie](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/cds/rpz_rmark.md).
Ce script concentre toutes les cartographies faites, des cartes basiques
aux représentation géographiques des clusters et prédictions.  
<br/>
[Autres](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/cds/graph_rmark.md).
Dans ce script sont réalisées les analyses de complétudes, ainsi que
quelques graphiques.

<br/> <br/> <br/>

## Scripts à paritr des données md50 (invertébrés benthiques):

[Préparation des
données](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/md50/prep_data_md50.md).
De même que pour *Croix-du-Sud*, c’est une mise en forme des données, et
correction des erreurs qui y sont présentes, qui aboutit au tableau
*md\_site\_gps* que l’on utilisera pour les analyses.

[Graphiques](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/md50/graphiques_md50.md).
Quelques représentations graphiques du jeu de données, qui sont un peu
complexes à réaliser, et sont donc dans un script à part.

[analyses de
réseau](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/md50/reseaux.md).
Réalisées à partir de tableau *md\_site\_gps*. Dans ce script sont
détaillées les analyses réalisées au niveau de l’espèce. D’autres ont
été faites au niveau du genre, mais n’aboutissant à rien de plus
intéressant elles ne sont pas détaillées.

[analyses de
corrélation](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/md50/correl_md50.md).
Ce sont les analyses de l’impact de la profondeur ainsi que d’autres
variables sur la détermination des clusters, dont découle la prédiction
de présence des clusters sur la zone de SPA.

[Cartographie](https://github.com/ArionRufus/bioregions_benthiques_SPA/blob/master/md50/carto_md50.md).
Représentation cartographique des analyses.

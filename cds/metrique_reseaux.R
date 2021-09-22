
NewclusterMetrics = function (
          db, #le jeu de données initial de présence d'espèces pour chaque site
          network = NULL, #clustering results
          site.field = colnames(db)[1], #Name of sites column in your database
          species.field = colnames(db)[2], #Name of species column in your database
          species.richness = NULL,
          site.area = NULL, # data.frame containing at least columns site name and site surface
          level = "lvl1" #sur quel level de clusters on veut les analyses
          ) 
{
  #creation du data.frame dans lequel on stockera tous les résultats:
  region.stats <- data.frame(
                    #les regions, = les clusters créés:
                    region = levels(network[, level]),
                    #nb.sites = combien on a de sites dans chaque cluster:
                    nb.sites = sapply(
                                levels(network[, level]), #pour chaque cluster,
                                function(x, net, database) {
                                  length(unique(database[which(database[, site.field] %in% 
                                  net$Name[which(net[, level] == x)]), site.field]))
                                }, net = network, database = db)
                    )
  
  if (!is.null(species.richness)) {
    region.stats = cbind(region.stats,
                         richesse = sapply(
                           levels(network[, level]),
                           function(x, net, database) {
                             sum(
                               database[which(database[,site.field] %in% net$Name[which(net[, level] == x)]), 
                                        species.richness])
                           }, net = network, database = db))}
  
  #si on precise la surface de chaque site:
  if (!is.null(site.area)) {
    region.stats <- data.frame(region.stats, area = sapply(levels(network[, 
                                                                          level]), function(x, net, surf) {
                                                                            sum(surf$area[which(surf$name %in% net$Name[which(net[, 
                                                                                                                                  level] %in% x)])])
                                                                          }, net = network, surf = site.area))
  }
  
  #tableau avec juste les différentes espèces pour l'instant:
  sp.stats <- data.frame(
    species = levels(as.factor(db[, species.field])))
  rownames(sp.stats) <- sp.stats$species
  
  for (sp in sp.stats$species) { #pour chaque espèce,
    #colonne cluster:
    #a quel cluster elle appartient
    sp.stats[sp, "cluster"] <- network[which(network$Name == sp), level]
    
    #quels sont les sites où elle est présente:
    site.occurrence.total <- unique(db[which(db[, species.field] == 
                                               sp), site.field])
    
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
    
    
    if (!is.null(site.area)) {
      sp.stats[sp, "Ri"] <- sum(site.area$area[which(site.area$name %in% 
                                                       site.occurrence.region)])
      sp.stats[sp, "Di"] <- sum(site.area$area[which(site.area$name %in% 
                                                       site.occurrence.total)])
      sp.stats[sp, "Ai"] <- sp.stats[sp, "Ri"]/region.stats$area[which(region.stats$region == 
                                                                         sp.stats[sp, "cluster"])]
      sp.stats[sp, "Fi"] <- sp.stats[sp, "Ri"]/sp.stats[sp, 
                                                        "Di"]
      sp.stats[sp, "IndVal"] <- sp.stats[sp, "Ai"] * 
        sp.stats[sp, "Fi"]
      sp.stats[sp, "DilVal"] <- sp.stats[sp, "Ai"] * 
        (1 - sp.stats[sp, "Fi"])
    }
  }
  
  
  #tableau avec chaque ligne représentant un site:
  site.stats <- data.frame(site = levels(as.factor(db[, site.field])))
  rownames(site.stats) <- site.stats$site
  
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
    
    
    if (!is.null(site.area)) {
      site.stats[site, "Rg"] <- sum(sp.stats$IndVal[which(sp.stats$species %in% characteristic.sp)]) - 
                                sum(sp.stats$DilVal[which(sp.stats$species %in% noncharacteristic.sp)])
      site.stats[site, "RRg"] <- site.stats[site, "Rg"]/length(sp.in.site)
    }
  }
  return(list(region.stats = region.stats, species.stats = sp.stats, 
              site.stats = site.stats))
}


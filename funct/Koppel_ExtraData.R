Koppel_Buurtgegevens <- function(pnt,
                                 shp = './Data/Buurt_2018/buurt2018.shp',
                                 xcl = './Data/Buurtinfo2.csv'
                                 
){
  source('./funct/Basics.R')
  
  ipak(c("sf", "sp", "rgdal"))
  epsg <- make_EPSG()
  projectie <- CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"])
  
  
  
  # data inlezen
  Buurt <- as(read_sf(shp), 'Spatial')
  Buurt@proj4string <- pnt@proj4string
  Buurt <- Buurt[, colnames(Buurt@data) == "BU_CODE"]
  
  Buurt_extra <- read.csv(xcl, sep = ";")
  Buurt@data <- Buurt@data %>% left_join(Buurt_extra)
  
  GM <- Buurt_extra[grepl("GM", Buurt_extra$BU_CODE), ]   
  GM <- data.frame(gm_naam = GM$gm_naam, 
                   GM_CODE = GM$BU_CODE,
                   a_hh_gem = GM$a_hh)
  
  
  
  ## koppel de data
  iii <- data.frame(pnt@data, over(pnt, Buurt))
  iv <- iii %>% left_join(GM)
  
  pnt <- SpatialPointsDataFrame(iv[,c("X", "Y")], iv, proj4string = CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"]))
  
  return(pnt)
  
  
}

Koppel_BAGgegevens <- function(PV
                               
){
  source('./funct/Basics.R')
  
  ipak(c("sf", "rgdal", "dplyr"))
  epsg <- make_EPSG()
  
  i <- list.files('./Data', pattern = "woningtype", full.names = T)
  ii <- lapply(i, function(t){
    x <- read.delim(file = t, sep = ";", dec = ".", header = T, stringsAsFactors=FALSE)
    x[, colnames(x) %in% c("HUISNUMMER", "HUISLETTER", "TOEVOEGING", "POSTCODE",
                            "GEBR_DOEL1", "BOUWJAAR", "OPPERVLAK", "FOOTPRINT", "EGMG", "WONINGTYPE")]
    })
  
  
  PV[["ID"]] <- 1:nrow(PV)
  
  BAG <- do.call(rbind, ii)  
  colnames(BAG)[colnames(BAG) == "HUISLETTER"] <- "HUISNUMMER_TOEVOEGING"
  
  PV$HUISNUMMER_TOEVOEGING <- gsub('[0-9]+|-| ', '', PV$HUISNUMMER_TOEVOEGING)
  BAG$HUISNUMMER_TOEVOEGING <- gsub('[0-9]+|-| ', '', BAG$HUISNUMMER_TOEVOEGING)
  
  PV$HUISNUMMER_TOEVOEGING[PV$HUISNUMMER_TOEVOEGING == ""] <- NA
  BAG$HUISNUMMER_TOEVOEGING[BAG$HUISNUMMER_TOEVOEGING == ""] <- NA
  
  BAG$HUISNUMMER_TOEVOEGING <- toupper(BAG$HUISNUMMER_TOEVOEGING)
  PV$HUISNUMMER_TOEVOEGING <- toupper(PV$HUISNUMMER_TOEVOEGING)
  
  PV$HUISNUMMER_TOEVOEGING[!PV$HUISNUMMER_TOEVOEGING %in% BAG$HUISNUMMER_TOEVOEGING] <- NA
  BAG$HUISNUMMER_TOEVOEGING[!BAG$HUISNUMMER_TOEVOEGING %in% PV$HUISNUMMER_TOEVOEGING] <- NA
  
  PV <- PV %>% left_join(BAG) %>% arrange(BOUWJAAR, BOUWJAAR_PV) %>% group_by(ID) %>%
    mutate(Rank = row_number()) %>% filter(Rank ==1)
  
  PV_klaar <- PV[!is.na(PV$WONINGTYPE),]
  PV_aanvul <- PV[is.na(PV$WONINGTYPE),]
  PV_aanvul <- PV_aanvul[,colnames(PV_aanvul) %in% c("HUISNUMMER", "POSTCODE", "WOONPLAATS","GEMEENTE", "X", "Y", 
                                                     "PV","BOUWJAAR_PV", "ID")]
  
  PV_aanvul <- PV_aanvul %>% left_join(BAG) %>% arrange(BOUWJAAR, BOUWJAAR_PV) %>% group_by(ID) %>%
    mutate(Rank = row_number()) %>% filter(Rank ==1)
  
  PV <- rbind(PV_klaar, PV_aanvul)
  
  var <- which(colnames(PV) %in% c("BOUWJAAR_PV", "BOUWJAAR"))
  PV[var] <- data.frame(sapply(PV[var], as.numeric))
  
  
  
  
  return(PV)
  
}

Koppel_Verkiezing <- function(pnt, 
                              xls = './Data/ps2019.csv'){
  
  ipak(c("dplyr", "dismo"))
  
  PC <- pnt@data %>% group_by(POSTCODE) %>% summarise(X = mean(X, na.rm = T),
                                                      Y = mean(Y, na.rm = T))
  PC <- PC[PC$Y > 370000,]
  Uitslag <- read.csv(file = xls, sep =";")
  colnames(Uitslag)[colnames(Uitslag) == "Forum.voor.Democratie_percentage"] <- "Perc_FvD" 
  colnames(Uitslag)[colnames(Uitslag) == "ChristenUnie_percentage"] <- "Perc_CU" 
  colnames(Uitslag)[colnames(Uitslag) == "GROENLINKS_percentage"] <- "Perc_GL" 
  colnames(Uitslag)[colnames(Uitslag) == "PVV..Partij.voor.de.Vrijheid._percentage"] <- "Perc_PVV" 
  colnames(Uitslag)[colnames(Uitslag) == "CDA_percentage"] <- "Perc_CDA" 
  colnames(Uitslag)[colnames(Uitslag) == "Partij.voor.de.Dieren"] <- "Perc_PVD" 
  colnames(Uitslag)[colnames(Uitslag) == "VVD_percentage"] <- "Perc_VVD" 
  colnames(Uitslag)[colnames(Uitslag) == "DENK_percentage"] <- "Perc_DENK" 
  
  Uitslag <- Uitslag[, grepl("Perc_|bureau_zip|bureau_label",colnames(Uitslag))]
  
  ####
  
  
  Uitslag_ID <- Uitslag %>% 
    mutate(bureau_zip = gsub(" ", "", bureau_zip)) %>%
    inner_join(PC, by = c("bureau_zip" = "POSTCODE")) %>%
    group_by(X, Y) %>% summarise(bureau_label = first(bureau_label))
  
  Uitslag_vlak <- voronoi(as.matrix(Uitslag_ID[, c("X", "Y")]))
  Uitslag_vlak@data <- Uitslag_ID
  Uitslag_vlak@proj4string <- pnt@proj4string
  
  Uitslag_vlak@data <- Uitslag_vlak@data %>% left_join(Uitslag)
  
  
  pnt@data <- data.frame(pnt@data, over(pnt, Uitslag_vlak)) 
  
  pnt@data$bureau_label <- NULL
  pnt@data$bureau_zip <- NULL
  
  return(pnt)
  
}

Koppel_inkomen <- function(pnt, 
                           shp = './Data/Buurt_2018/wijk_2018.shp',
                           xls = './Data/inkomen.csv'){
  
  ipak(c("dplyr"))
  ipak(c("sf", "sp", "rgdal"))
  epsg <- make_EPSG()
  projectie <- CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"])
  
  
  inkomen <- read.csv(file = xls, sep =";")
  
  wijk <- as(read_sf(shp), 'Spatial')
  wijk@proj4string <- pnt@proj4string
  wijk@data <- wijk@data[, colnames(wijk@data) %in% c("WK_CODE", "WK_NAAM")] %>% left_join(inkomen)
  wijk@data <-  wijk@data[, colnames(wijk@data) != "WK_NAAM"]
  
  ## koppel de data
  iv <- data.frame(pnt@data, over(pnt, wijk)) 
  pnt <- SpatialPointsDataFrame(iv[,c("X", "Y")], iv, proj4string = CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"]))
  
  return(pnt)
  
}

Koppel_Verhuis <- function(pnt, 
                           xls = './Data/verhuis.csv'){
  ipak(c("dplyr"))
  ipak(c("sf", "sp", "rgdal"))
  epsg <- make_EPSG()
  projectie <- CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"])
  
  verhuis <- read.csv(file = xls, sep =";")
  verhuis[,2:4] <- sapply(verhuis[,2:4], as.numeric)
  
  colnames(verhuis)[colnames(verhuis) == "BU_CODE"] <- "GM_CODE"
  iv <-  pnt@data %>% left_join(verhuis)
  pnt <- SpatialPointsDataFrame(iv[,c("X", "Y")], iv, proj4string = CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"]))
  
  return(pnt)
}

Bereken_PVDist <- function(PV, straal = 150, PC_num = 5, folder = './Data/PV_trace', Cores = detectCores()-1, Jaar = 2016, sub = F){
  
  Maak_netwerk <- function(pnt_ii, pnt_iii, rij, straal = 150){
    
    zelf <- pnt_ii[rij,]
    ii <- pnt_iii[pointDistance(pnt_iii@coords, zelf@coords, lonlat = F) < straal,]
    
    if (max(ii$PV, na.rm = T) > 0 & nrow(ii) > 1) {
      
      # dubbelingen verwijderen
      iii <- ii@data %>% group_by(X, Y) %>% arrange(-PV) %>% mutate(rank = row_number()) %>% filter(rank == 1) ##Dubbele punten verwijderen
      ii <- ii[ii$ID %in% iii$ID |ii$ID %in% zelf$ID,]
      
      #### de trace kan niet gebeuren  naar panelen met hetzelfde aanlegjaar
      ii$PV[which(ii$BOUWJAAR_PV + 1 >= zelf$BOUWJAAR_PV)] <- 0
      
      ii[["Rij"]] <- row_number(ii$ID)
      plaats <- ii$Rij[ii$ID == zelf$ID]
      
      c <- gabrielneigh(ii@coords)
      links <- data.frame(from = c$from, to = c$to, type = "Buur", weight = 1,
                          length = unlist(lapply(row_number(c$from), function(i){
                            pointDistance(ii@coords[ii$Rij == c$from[i]], ii@coords[ii$Rij == c$to[i]], lonlat = F)
                          })))
      
      
      net <- graph_from_data_frame(d=links, vertices=ii@data[,c("Rij", "PV")], directed=F) 
      a <- get.vertex.attribute(net, "PV")
      a[plaats] <- 0 # als het punt zelf 0 is dit corrigeren
      
      return(
        data.frame(ID = as.numeric(zelf$ID),
                   Aantal_Buur = nrow(ii),
                   Aantal_PV = (sum(ii$PV, na.rm = T) - zelf$PV),
                   PV_percentage = signif(
                     (sum(ii$PV, na.rm = T) - zelf$PV)/
                       nrow(ii),4), 
                   PV_Afstand_Aantal = min(shortest.paths(net, to = plaats, v=V(net)[a==1], weights=links$weight)), 
                   PV_Afstand_Lengte = round(min(shortest.paths(net, to = plaats, v=V(net)[a==1], weights=links$length)),0),
                   stringsAsFactors = F))
    } else if (nrow(ii) > 1){
      return(
        data.frame(ID = as.numeric(zelf$ID),
                   Aantal_Buur = nrow(ii),
                   Aantal_PV = (sum(ii$PV, na.rm = T) - zelf$PV),
                   PV_percentage = 0, 
                   PV_Afstand_Aantal = Inf, 
                   PV_Afstand_Lengte = Inf,
                   stringsAsFactors = F))
    }
  }
  
  source('./funct/Basics.R')
  ipak(c("dplyr", "sf", "sp", "geometry", "rgdal", "SDraw", "igraph", "rgdal", "spdep", "raster", "rgeos", "DMwR", "parallel", "pbapply"))
  
  epsg <- make_EPSG()
  projectie <- CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"])
  
  ###
  
  pnt <- SpatialPointsDataFrame(PV[,c("X", "Y")], PV, proj4string = CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"]))
  
  pnt@data[["ID"]] <- 1:length(pnt@data$POSTCODE)
  pnt@data[["ITEM"]] <- substr(pnt@data$POSTCODE, 0,PC_num)
  
  ###
  
  klaar <- list.files(folder)
  klaar <- gsub('.rds', '', klaar)
  
  #i <- pnt@data %>% group_by(X, Y) %>% arrange(-PV) %>% mutate(rank = row_number()) %>% filter(rank == 1)
  
  ii <- pnt@data[!is.na(pnt@data$ITEM) ,] %>% #& !pnt@data$PC %in% Klaar 
    group_by(ITEM) %>% 
    summarise(Aantal = n(), PV = sum(PV, na.rm = T)) %>%
    mutate(perc = PV/Aantal) %>%
    mutate(Totaal = cumsum(Aantal)) %>%
    filter(Aantal > 4 & !ITEM %in% klaar & !ITEM %in% c("", "NA", NA))
  
  if ( sub == T){  ii <- ii[sample(1:nrow(ii), 40),] }
  
  pnt_i <- pnt[pnt$ITEM %in% ii$ITEM,] #pnt$ID %in% i$ID & 
  
  ### recent geplaatste panelen kunnen geen peer-effect opwekken
  pnt_i$PV[pnt_i$BOUWJAAR_PV > Jaar] <- NA
  
  
  Uitvoer <- 1:nrow(ii)
  
  if ( length(Uitvoer) < 1){print( paste("No new connetions to compute, look at " ,folder, " for the files"))
  } else{
    print(paste0("ToDo: ", max(Uitvoer), " computation(s)"))
  }
  
  ##### loop over items
  cl <- makePSOCKcluster(Cores)
  setDefaultCluster(cl)
  
  clusterExport(NULL, c('Maak_netwerk', 'ii', "pnt_i", "straal","Uitvoer", "folder",
                        "gabrielneigh", "graph_from_data_frame", "get.vertex.attribute", 
                        "shortest.paths","V", "pointDistance", "row_number", "gBuffer"), envir=environment())
  
  system.time(#ITEM <- 
    pblapply(Uitvoer, function(item){
      library(dplyr)
      
      pnt_ii <- pnt_i[pnt_i$ITEM == ii$ITEM[item],]
      pnt_iii <- pnt_i[gBuffer(pnt_ii, width = straal),]
      
      i <- lapply(1:nrow(pnt_ii), function(rij){ try(
        Maak_netwerk(pnt_ii, pnt_iii, rij, straal =150) 
        )})
      b <- do.call(rbind, i)
      gc()
      saveRDS(b, paste0(folder,"/", ii$ITEM[item], ".rds"))
    }, cl = cl))
  
  #ITEM <- parLapply(cl, Uitvoer, function(item){
  #pnt_ii <- pnt_i[pnt_i$ITEM == ii$ITEM[item],]
  #pnt_iii <- pnt_i[gBuffer(pnt_ii, width = straal),]
  #i <- lapply(1:nrow(pnt_ii), function(rij){ try(Maak_netwerk(pnt_ii, pnt_iii, rij, straal =150) )})
  #b <- do.call(rbind, i)
  #gc()
  #saveRDS(b, paste0('./Data/PV_buurt/', ii$ITEM[item], ".rds"))
  #})
  
  stopCluster(cl)
  ITEM <- list.files(folder, full.names = T)
  ITEM <- ITEM[!grepl("/.rds|NA, ", ITEM)]
  ITEM <- lapply(ITEM, readRDS)
  
  ii <- as.numeric(median(as.numeric(summary(ITEM)[,1]), na.rm = T))
  ITEM <- ITEM[    which(unlist(sapply(ITEM, function(i){ncol(i) == ii})))]
  
  iii <- do.call(rbind, ITEM)
  iii$ID <- as.numeric(iii$ID)
  iii <- iii[!is.na(iii$ID),]
  iii <- iii %>% group_by(ID) %>% summarise(Aantal_Buur = min(Aantal_Buur, na.rm = T), 
                                            PV_percentage = max(PV_percentage, na.rm = T),
                                            PV_Afstand_Aantal = min(PV_Afstand_Aantal, na.rm = T),
                                            PV_Afstand_Lengte = min(PV_Afstand_Lengte, na.rm = T))
  
  
  iv <- pnt@data %>% left_join(iii)
  pnt <- SpatialPointsDataFrame(iv[,c("X", "Y")], iv, proj4string = CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"]))
  
  #### aanvullen
  #Aanvul <- pnt@data[is.na(pnt$PV_Afstand_Aantal), !colnames(pnt@data) %in% c("Aantal_Buur", "PV_percentage", "PV_Afstand_Aantal", "PV_Afstand_Lengte")]
  #Compleet <- pnt@data[!is.na(pnt$PV_Afstand_Aantal), ]
  
  #na <- Aanvul[,colnames(Aanvul) %in% c("X", "Y")] %>% left_join(Compleet) %>% group_by(X, Y) %>%
    #summarise(Aantal_Buur = max(Aantal_Buur, na.rm = T),
    #          PV_percentage = max(PV_percentage, na.rm = T),
    #          PV_Afstand_Aantal = max(PV_Afstand_Aantal, na.rm = T),
    #          PV_Afstand_Lengte = max(PV_Afstand_Lengte, na.rm = T))
  
  #Aanvul <- Aanvul %>% left_join(na) 
  
  #PV <- rbind(Aanvul, Compleet)
  
  #pnt <- SpatialPointsDataFrame(PV[,c("X", "Y")], PV, proj4string = CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"]))
  
  return(pnt)
}

Bereken_autocorrelatie <- function(pnt){
  
  
  ii <- pnt@data %>% group_by(BU_CODE) %>% filter(!is.na(BU_CODE)) %>%
    summarise(Aantal = n(), PV = sum(PV, na.rm = T)) %>%
    mutate(perc = PV/Aantal) %>% filter(perc > 0.01 & Aantal > 4)
  
  buurt <- ii$BU_CODE[ii$Aantal > quantile(ii$Aantal, 1/4) &ii$Aantal < quantile(ii$Aantal, 3/4)]
  
  i <- lapply(1:length(buurt), function(i){
    
    if ( (i/10)%%1 == 0){
      print(length(buurt) - i)
    }
    
    xxx <- pnt[pnt$BU_CODE == buurt[i],]
    
    if (max(xxx$PV) == 1 ){
      Cor <- correlog(xxx@coords[,1], 
                      xxx@coords[,2],
                      xxx$PV, latlon =F, increment=25, resamp = 0)
      data.frame(Cor$correlation[1],
                 Cor$correlation[2],
                 Cor$correlation[3],
                 Cor$correlation[4],
                 Cor$correlation[5],
                 Cor$correlation[6],
                 Cor$correlation[7],
                 Cor$correlation[8],
                 Cor$correlation[9],
                 Cor$correlation[10],
                 Cor$correlation[11]
      )
      
    }
  })
  
  ii <- do.call(rbind, i)
  iii <- apply(ii, 2, mean)
  iv <- data.frame(x = 1:length(iii) * 25, y = iii)
  plot(iv$x[-1], iv$y[-1], ylab = " Spatial autocorrelation", xlab = "Distance (m)", type = "o", main = "Spatial correlation between residents adopting PV")
  
}

Koppel_SES <- function(pnt, xls = './Data/SES.csv' ){
  
  SES <- read.csv(file = xls, sep =";")
  SES <- SES[, colnames(SES) %in% c("pcnr", "statusscore17")]
  SES$pcnr <- as.character(SES$pcnr)
  
  pnt@data[["pcnr"]] <- as.character(substr(pnt@data$POSTCODE, 1,4))
  
  pnt@data <-  pnt@data %>% left_join(SES)
  return(pnt)
  
}





Bewerk_Data <- function(pnt){
  
  ipak(c("dplyr"))
  ###########
  if ( class(pnt) == "SpatialPointsDataFrame"){
    pnt_glm <- pnt@data
  } else (pnt_glm <- pnt )
  
  Dubbel <- pnt_glm %>% mutate(xy = paste(X, Y, sep = "_")) %>% group_by(xy) %>% summarise(AaantalOpXY = n()) %>% mutate(DubbelAdres = ifelse(AaantalOpXY > 1, "Ja", "Nee"))
  pnt_glm <- pnt_glm %>% mutate(xy = paste(X, Y, sep = "_")) %>% left_join(Dubbel)
  
  pnt_glm$WONINGTYPE <- as.character(pnt_glm$WONINGTYPE)
  pnt_glm[["Hoogbouw"]] <- "Nee"
  pnt_glm$Hoogbouw[pnt_glm$WONINGTYPE == "1"] <- "Ja"
  
  
  pnt_glm <- pnt_glm[,!colnames(pnt_glm) %in% c("DUURZAME_ENERGIE", "STRAATNAAM", "HUISNUMMER", "HUISNUMMER_TOEVOEGING",
                                        "POSTCODE", "WOONPLAATS", "GEMEENTE", "X", "Y", "BU_CODE", "gwb_code_10",
                                         "gwb_code_8", "regio", "gm_naam", "recs", "ind_wbi", "WK_CODE", "GM_CODE",
                                         "bureau_label", "bureau_zip", "X.1", "Y.1")]
  
  # omrekenen naar percentage
  pnt_glm[, colnames(pnt_glm) %in% c("a_man", "a_vrouw", "a_00_14", "a_15_24", "a_25_44", "a_45_64", "a_65_oo",
                                     "a_ongeh", "a_gehuwd", "a_gesch", "a_verwed", "a_w_all", "a_nw_all",
                                     "a_marok", "a_antaru", "a_suri", "a_tur", "a_ov_nw" )] <- 
    
    pnt_glm[, colnames(pnt_glm) %in% c( "a_man", "a_vrouw", "a_00_14", "a_15_24", "a_25_44", "a_45_64", "a_65_oo",
                                        "a_ongeh", "a_gehuwd", "a_gesch", "a_verwed", "a_w_all", "a_nw_all",
                                        "a_marok", "a_antaru", "a_suri", "a_tur", "a_ov_nw" )]/pnt_glm$a_inw
  
  ########################
  
  var <- which(!colnames(pnt_glm) %in% c("GEBR_DOEL1", "WONINGTYPE", "DubbelAdres", "Hoogbouw"))
  pnt_glm[var] <- data.frame(sapply(pnt_glm[var], as.numeric))
  
  pnt_glm <- do.call(data.frame,lapply(pnt_glm, function(x){
    replace(x, is.infinite(x),NA) }))
  
  ### Groepen maken #################
  ##### Groepen inkomen #############
  i <- pnt_glm %>% dplyr::select(Inkomen_gemiddeld, PV) %>%
    group_by(Inkomen_gemiddeld) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>%
    filter(!is.na(Inkomen_gemiddeld))
  
  ii <- kmeans(i, 2, nstart = 5)
  iii <- data.frame(i, cluster = ii$cluster) %>%
    group_by(cluster) %>% summarise(Dempel = max(Inkomen_gemiddeld))
  drempel <- min(iii$Dempel)
  
  pnt_glm[["Inc_gr"]] <- "Inc_low"
  pnt_glm$Inc_gr[pnt_glm$Inkomen_gemiddeld > drempel] <- "Inc_high"
  
  pnt_glm <- pnt_glm %>%
    mutate(Inc_hi = ifelse(Inc_gr == "Inc_high", Inkomen_gemiddeld, 0)) %>%
    mutate(Inc_low = ifelse(Inc_gr == "Inc_low", Inkomen_gemiddeld, 0))
  
  #### Groepen bouwjaar huis ########
  i <- pnt_glm %>% dplyr::select(BOUWJAAR, PV) %>% filter(BOUWJAAR < 2019 & BOUWJAAR > 1920) %>%
    group_by(BOUWJAAR) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>%
    filter(!is.na(BOUWJAAR))
  
  ii <- kmeans(i, centers = 2)
  iii <- data.frame(i, cluster = ii$cluster) %>%
    group_by(cluster) %>% summarise(Dempel = max(BOUWJAAR))
  
  drempel <- min(iii$Dempel)
  
  pnt_glm[["House_gr"]] <- "House_1"
  pnt_glm$House_gr[pnt_glm$BOUWJAAR > drempel] <- "House_2"
  pnt_glm$House_gr[pnt_glm$BOUWJAAR > 2006] <- "House_3"
  
  ####### centreer waarde #################################
  
  ind <- sapply(pnt_glm, is.numeric)
  ind[which(names(ind) == "PV")] <- F
  pnt_glm[ind] <- lapply(pnt_glm[ind], scale)
  
  ##########################################3
  
  
  pnt_glm[["P_verhuis"]] <- (pnt_glm$Verhuis_kom + pnt_glm$Verhuis_ga + pnt_glm$Verhuis_blijf) /
    pnt_glm$a_hh_gem
  
  pnt_glm[["P_1p_hh"]] <- pnt_glm$a_1p_hh / pnt_glm$a_hh
  pnt_glm[["P_hh_z_k"]] <- pnt_glm$a_hh_z_k / pnt_glm$a_hh
  pnt_glm[["P_hh_m_k"]] <- pnt_glm$a_hh_m_k / pnt_glm$a_hh
  pnt_glm[["P_inkont"]] <- pnt_glm$a_inkont / pnt_glm$a_inw
  
  colnames(pnt_glm)[colnames(pnt_glm) == "Inkomen_gemiddeld"] <- "g_inkom"
  colnames(pnt_glm)[colnames(pnt_glm) == "BOUWJAAR"] <- "Bjr_h"
  colnames(pnt_glm)[colnames(pnt_glm) == "BOUWJAAR_PV"] <- "Bjr_pv"
  colnames(pnt_glm)[colnames(pnt_glm) == "statusscore17"] <- "SES"
  colnames(pnt_glm)[colnames(pnt_glm) == "PV_Afstand_Aantal"] <- "PV_dist"
  colnames(pnt_glm)[colnames(pnt_glm) == "PV_Afstand_Lengte"] <- "PV_nn_afst"
  colnames(pnt_glm)[colnames(pnt_glm) == "PV_percentage"] <- "PV_nn_perc"
  
  colnames(pnt_glm)[colnames(pnt_glm) == "GEBR_DOEL1"] <- "GEBRUIKSDOEL"
  colnames(pnt_glm)[colnames(pnt_glm) == "Inkomen_Laagste40"] <- "Inkom_L40"
  colnames(pnt_glm)[colnames(pnt_glm) == "Inkomen_Laagste20"] <- "Inkom_L20"
  
  pnt_glm$P_1p_hh[pnt_glm$P_1p_hh > 1] <-1 
  pnt_glm$P_hh_m_k[pnt_glm$P_hh_m_k > 1] <-1 
  pnt_glm$P_hh_z_k[pnt_glm$P_hh_z_k > 1] <-1 
  pnt_glm$a_ongeh[pnt_glm$a_ongeh > 1] <-1 
  
  
  colnames(pnt_glm) <- gsub("P_", "p_", colnames(pnt_glm))
  pnt_glm[, grepl("p_",colnames(pnt_glm))] <- pnt_glm[, grepl("p_",colnames(pnt_glm))]/100
  pnt_glm$p_hh_m_k <- pnt_glm$p_hh_m_k * 100 
  pnt_glm$p_hh_z_k <- pnt_glm$p_hh_z_k * 100 
  pnt_glm$p_1p_hh <- pnt_glm$p_1p_hh * 100 
  pnt_glm$p_verhuis <- pnt_glm$p_verhuis * 100 
  
  #pnt_glm$PV_nn_perc[pnt_glm$PV_nn_perc > 0.9] <- NA
  #pnt_glm$a_w_all[pnt_glm$a_w_all > 0.5] <- NA
  
  pnt_glm <- pnt_glm[,which(colnames(pnt_glm) %in% c("PV",
                                                     
                                                     # gelaslacht
                                                     "a_man", "a_vrouw", 
                                                     
                                                     # leeftijd
                                                     "a_00_14", "a_15_24", "a_25_44", "a_45_64", "a_65_oo",
                                                     
                                                     #burgelijke status
                                                     "a_ongeh", "a_gehuwd", "a_gesch", "a_verwed", 
                                                     
                                                     # etniciteit
                                                     "a_w_all", "a_nw_all", #"a_marok", "a_antaru", "a_suri", "a_tur", "a_ov_nw"
                                                     
                                                     #Type huishouden
                                                     "p_1p_hh", "p_hh_z_k", "p_hh_m_k", "g_hhgro",
                                                     
                                                     # Stedelijkheid
                                                     "bev_dich", "a_woning", "ste_mvs",          
                                                     
                                                     #Woning dingen
                                                     "g_woz", "p_1gezw", "p_mgezw", "p_bewndw", "p_leegsw", "p_koopw",
                                                     "p_huurw", "p_wcorpw","p_ov_hw","p_e_o_w",
                                                     
                                                     #Inkomsten
                                                     #"p_inkont", "g_ink_po", "g_ink_pi", "p_ink_li", "p_ink_hi", 
                                                     #"p_n_act", "p_hh_li", "p_hh_hi", "p_hh_lkk","p_hh_osm",
                                                     
                                                     "SES",
                                                     
                                                     "Inc_gr", "Inkom_L40", "Inkom_L20",
                                                     
                                                     "g_inkom", "Inc_hi", "Inc_low",
                                                     
                                                     #"a_soz_wb", "a_soz_ao", "a_soz_ww", "a_soz_ow",
                                                     
                                                     # Ciriminaliteit
                                                     "g_wodief", "g_vernoo", "g_gewsek",
                                                     
                                                     # Woning
                                                     "Bjr_h", "Bjr_pv", "House_gr",
                                                     
                                                     #Stemmen
                                                     "Perc_PVD", "Perc_CDA", "Perc_VVD", "Perc_CU", "Perc_FvD", "Perc_GL", 
                                                     "Perc_PVV", "Perc_DENK", 
                                                     
                                                     #residentiele mobiliteit
                                                     "p_verhuis",
                                                     
                                                     #Peer influence
                                                     "PV_dist", "PV_nn_afst", "PV_nn_perc", "PV_There", "Aantal_Buur",
                                                     
                                                     # catagorie
                                                     "GEBRUIKSDOEL", "OPPERVLAK", "EGMG", "WONINGTYPE", "FOOTPRINT", "Hoogbouw",
                                                     
                                                     "AaantalOpXY", "DubbelAdres"
                                                     ))]
  
  ###################
  
  pnt_glm[["a_mig"]] <- pnt_glm$a_nw_all + pnt_glm$a_w_all
  
  pnt_glm$GEBRUIKSDOEL <- as.character(pnt_glm$GEBRUIKSDOEL)
  pnt_glm$GEBRUIKSDOEL[is.na(pnt_glm$GEBRUIKSDOEL) | pnt_glm$GEBRUIKSDOEL == "<NA>"] <- "Onbekend"
  
  #### NA wegwerken ####
  pnt_glm$PV_dist[is.na(pnt_glm$PV_dist)] <- 50
  
  
  return(pnt_glm)
  
  
}

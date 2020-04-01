############# Inladen ##################
plaatjes <- F
NieuweData <- F

############# Variabele ##################
demografisch <- c("a_00_14",  "a_45_64", "a_65_oo", "a_mig", "a_man",#"a_w_all", "a_nw_all",#"a_25_44","a_15_24" "a_w_all",
                  "p_1gezw","a_ongeh", "p_verhuis" #,#"a_vrouw","a_ongeh", "g_hhgro"
                  #"p_hh_m_k"#, , "p_hh_z_k", "p_mgezw"
)
Controle <- c("PV","House_gr", "p_koopw",  "Inc_gr", "Inc_low", "Inc_hi", "g_woz", "ste_mvs", "Hoogbouw") #"Bjr_h","SES","Inkom_L20",
PV_dingen <- c("PV_dist") # "PV_nn_perc", "PV_dist_sqrt","PV_nn_afst", "Aantal_Buur", "PV_There", "Aantal_Buur", "Inkom_L40", "PV_nn_perc"

############# Model ##################

source('./funct/Basics.R')
source('./funct/ImportPanels.R')
source('./funct/bewerk_data.R')

options(scipen=999)

ipak(c("ggplot2","dplyr", "mapview", "rgdal", "ResourceSelection", "glmulti", "caret", "tidyr", "arm", "ggExtra", 
       "DescTools", "PerformanceAnalytics", "ggalt")) #"sf", "sp", 

# data inladen
if (NieuweData == T){
  pnt <- PVladen(New = F)
  pnt_glm <- Bewerk_Data(pnt@data)
}

if (plaatjes == T){
  pnt_glm %>% group_by(PV_dist) %>% summarise(Aant_PV = sum(PV),
                                                   Aant_tot = n()) %>% 
    #mutate(Aant = Aant / sum(Aant)) %>%
    mutate(PV_dist = ifelse(is.na(PV_dist), Inf, PV_dist)) %>%
    filter(PV_dist < 10 |is.infinite(PV_dist)) %>%
    tidyr::gather(Veld, Waarde, -PV_dist) %>%
    ggplot(aes(as.character(PV_dist ), Waarde, fill =  Veld)) +  
    geom_bar(stat = "identity") + 
    labs(title = "Homes that adopted PV", subtitle = "Distance to PV via neighbourhood network")+
    ylab("% of total") +
    xlab("Distance") +
    theme_bw()
  
  
  
  pnt_glm %>% filter(PV == 0 ) %>% group_by(PV_dist) %>% summarise(Aant = n()) %>%
    mutate(Aant = Aant / sum(Aant)) %>%
    mutate(PV_dist = ifelse(is.na(PV_dist), Inf, PV_dist)) %>%
    filter(PV_dist < 10 |is.infinite(PV_dist)) %>%
    
    ggplot(aes(as.character(PV_dist), Aant  )) +  
    geom_bar(stat = "identity") + 
    labs(title = "Homes that not adopted PV", subtitle = "Distance to PV via neighbourhood network") +
    ylab("% of total") +
    xlab("Distance") +
    theme_bw()
    
 
  # perc PV
  ggMarginal(
    pnt_glm %>% 
      mutate(PV_nn_perc = round(PV_nn_perc, 2)) %>%
      group_by(PV_nn_perc) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>% #filter(!is.na(Waarde)) %>%
      ggplot(aes(x = PV_nn_perc, y = Perc)) +
      geom_point() +
      theme_bw() +
      geom_smooth(method = lm, se = FALSE, col = "black") +
      xlab("Perc PV 150m") +
      ylab("% PV") +
      labs(shape = "Income group"), 
    type="histogram")
  
  
  # Inkomen
  ggMarginal(
    pnt_glm %>% 
      group_by(g_inkom, Inc_gr) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>% #filter(!is.na(Waarde)) %>%
      ggplot(aes(x = g_inkom, y = Perc, shape  = Inc_gr)) +
      geom_point() +
      scale_shape_manual(values = c(2, 5)) +
      geom_smooth(method = lm, se = FALSE, col = "black") +
      theme_bw() +
      xlab("Household income") +
      ylab("% PV") +
      labs(shape = "Income group"), 
    type="histogram")
  
  # Bouwjaar
  ggMarginal(
    pnt_glm %>% 
      group_by(Bjr_h, House_gr) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>% filter(Bjr_h < 2018) %>%
      ggplot(aes(x = Bjr_h, y = Perc, shape  = House_gr)) +
      geom_point() +
      scale_shape_manual(values = c(0,1,2)) +
      #geom_smooth(method = lm, se = FALSE, col = "black") +
      theme_bw() +
      xlab("Construction year") +
      ylab("% PV") +
      geom_encircle(s_shape=2) + 
      labs(shape = "House group"), 
    type="histogram")
 

# PV_dist
ggMarginal(
  pnt_glm %>% 
    mutate(PV_dist = ifelse(PV_dist %in% c(26,32), round(PV_dist/5,0)*5, PV_dist)) %>%
    
    group_by(PV_dist) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>%
    ggplot(aes(x = PV_dist, y = Perc)) +
    geom_point() +
    #scale_shape_manual(values = c(0,1,2)) +
    #geom_smooth(method = lm, se = FALSE, col = "black") +
    theme_bw() +
    xlab("PV_dist") +
    ylab("% PV") +
    #geom_encircle(s_shape=2) + 
    labs(shape = "House group"), 
    type="histogram")

  
  ############# Analyse ##############################
  df <- pnt_glm %>% dplyr::select(PV, a_man, a_vrouw, a_00_14, a_15_24, a_25_44, a_45_64, a_65_oo, a_ongeh, a_gehuwd, a_gesch,
                                    a_verwed, a_w_all, a_nw_all, g_hhgro, bev_dich, a_woning, g_woz, p_1gezw, p_mgezw, p_bewndw,
                                    p_leegsw, p_koopw, p_huurw, p_wcorpw, p_ov_hw, p_e_o_w, 
                                    #g_wodief, 
                                    g_vernoo, g_gewsek, ste_mvs, 
                                    g_inkom, Bjr_h, 
                                    #Perc_PVD, Perc_CDA, Perc_VVD, Perc_CU, Perc_FvD, Perc_GL, Perc_PVV, Perc_DENK,
                                    SES, PV_nn_perc, PV_dist, PV_dist_sqrt ,PV_nn_afst, p_verhuis, p_1p_hh, p_hh_z_k, p_hh_m_k) %>% 
    tidyr::gather("Veld", "Waarde", -PV) %>% 
    dplyr::filter(!is.na(Waarde) & !is.infinite(Waarde)) %>% 
    mutate(Waarde = round(Waarde,2)) %>% 
    mutate(Waarde = ifelse(Veld == "Bjr_h", round(Waarde,0), signif(Waarde,2))) %>% 
    group_by(Veld, Waarde) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>%
    mutate(Perc = ifelse(Perc > 1, 1, Perc))
  
  
  niet <- c("bev_dich", "a_man","a_vrouw", "p_e_o_w","g_vernoo", "PV_dist", "p_bewndw", "a_verwed")
  demografisch <- c("a_00_14", "a_15_24", "a_25_44", "a_45_64", "a_65_oo", "a_gehuwd", "a_gesch",  "a_nw_all",
                    "a_ongeh", "a_w_all", "g_hhgro", "p_1gezw","bev_dich", "a_man","a_vrouw",
                    "p_1p_hh", "p_hh_m_k", "p_hh_z_k", "p_mgezw")
  
  Overig <- c("ste_mvs", "p_verhuis")
  Controle <- c("Bjr_h", "p_verhuis", "g_woz", "p_huurw", "p_koopw", "ste_mvs", "p_wcorpw", "SES")
  Politiek <- c("Perc_PVD", "Perc_CDA", "Perc_VVD", "Perc_CU", "Perc_FvD", "Perc_GL", "Perc_PVV", "Perc_DENK")
  PV_dingen <- c("PV_nn_afst", "PV_nn_perc", "PV_150", "PV_dist_sqrt", "PV_dist")

  par(mfrow=c(4,4))
  for (i in demografisch){plotM(i)}
  par(mfrow=c(2,4))
  for (i in Controle){plotM(i)}
  for (i in Politiek){plotM(i)}
  
  par(mfrow=c(1,2))
  for (i in Overig){plotM(i)}
  
  ############# PV dist #####
  
  df <- pnt_glm %>% dplyr::select(PV, PV_dist, GEBRUIKSDOEL) %>% 
    group_by(GEBRUIKSDOEL, PV_dist) %>% summarise(Perc = sum(PV, na.rm = T) / n()) %>%
    mutate(Perc = ifelse(Perc > 1, 1, Perc))
  
  par(mfrow=c(4,3))
  
  for (iii in c("bijeenkomstfunctie", "gezondheidszorgfunctie", "industriefunctie", "kantoorfunctie",#"logiesfunctie",
                "Onbekend", "onderwijsfunctie", "overige gebruiksfunctie", "sportfunctie", "winkelfunctie", "woonfunctie" )){
    
    df.temp<-df[df$GEBRUIKSDOEL==iii,]
    df.temp <- df.temp[df.temp$Perc < quantile(df.temp$Perc, 0.95, na.rm = T),]
    plot(df.temp$PV_dist,df.temp$Perc,
         xlab=iii,ylab="%PV")
    #abline(lm(df.temp$Perc~df.temp$Waarde))
  }
}

############# Correlatie #######################

pnt_glm_i <- pnt_glm

tmp <- pnt_glm_i[, colnames(pnt_glm_i) %in% c(Controle, demografisch, PV_dingen) ]
ind <- sapply(tmp, is.numeric)

#corrplot(tmp[ind])
#cor(tmp[ind], use="complete.obs", method="spearman") 

mcor <-cor(tmp[ind],  use = "complete.obs")
lower<-mcor
lower[lower.tri(mcor, diag=TRUE)]<-""

lower<-data.frame(lower, stringsAsFactors = F)

DESCRIPTIVE_STATISTICS  <- tmp[ind] %>%#pnt_glm_i[, which(colnames(pnt_glm_i) %in% Controle |
                                              # colnames(pnt_glm_i) %in% demografisch |
                                              # colnames(pnt_glm_i) == "g_inkom")] %>% 
  tidyr::gather("Variable", "Value") %>%
  group_by(Variable) %>% summarise(mean = signif(mean(Value, na.rm = T),2),
                                   min = signif(min(Value, na.rm = T),2),
                                   max = signif(max(Value, na.rm = T),2),
                                   sd = signif(sd(Value, na.rm = T),2))

tb <- DESCRIPTIVE_STATISTICS %>%
  tidyr::gather("Veld", "Waarde", -Variable) %>%
  tidyr::spread(Variable, Waarde)
  
rownames(tb) <- tb$Veld
tb$Veld <- NULL

DESCRIPTIVE_STATISTICS <-rbind(lower, tb)
write.table(DESCRIPTIVE_STATISTICS, file = './Data/Desctipt.txt', sep = ";")


############# GLM ##############
fit_null <- glm(PV~1, data=pnt_glm_i, family="binomial")


# Controle 
fit_Conrole <- glm(PV~.,
                   data=pnt_glm_i[, colnames(pnt_glm_i) %in% Controle],
                   family=binomial(),
                   na.action=na.exclude)
reg_Conrole <- summary(fit_Conrole)
R2_Cont <- data.frame(
  Var = "R_2", SE.x = as.numeric(1-logLik(fit_Conrole)/logLik(fit_null)))

# demografisch 

fit_demografisch <- glm(PV~.,
                        data=pnt_glm_i[, colnames(pnt_glm_i) %in% c(Controle, demografisch)],
                        family=binomial(),
                        na.action=na.exclude)
reg_demografisch <- summary(fit_demografisch)
R2_dem <- data.frame(
  Var = "R_2", SE.y = as.numeric(1-logLik(fit_demografisch)/logLik(fit_null)))

# PEER 
fit_PEER <- glm(PV~.,
                data=pnt_glm_i[, colnames(pnt_glm_i) %in% c(Controle, demografisch, PV_dingen)],
                family=binomial(link = 'logit'),
                na.action=na.exclude)
reg_PEER <- summary(fit_PEER)
R2_PEER <- data.frame(
  Var = "R_2", SE = as.numeric(1-logLik(fit_PEER)/logLik(fit_null)))

############# Samenvoegen ###################

reg <- rbind(
  data.frame(Model = "A", data.frame(Var = rownames(reg_Conrole$coefficients), reg_Conrole$coefficients)),
  data.frame(Model = "B", data.frame(Var = rownames(reg_demografisch$coefficients), reg_demografisch$coefficients)),
  data.frame(Model = "C", data.frame(Var = rownames(reg_PEER$coefficients), reg_PEER$coefficients)))

colnames(reg) <- c( "Model", "Var","B", "SE", "z.value", "P")

reg1 <- reg %>% 
  mutate(B = round(B, 4)) %>%
  mutate(SE = round(SE, 4)) %>%
  mutate(z.value = round(z.value, 2)) %>%
  mutate(Sig = ifelse(P < 0.001, "***", 
                      ifelse(P < 0.01,"**",
                             ifelse(P < 0.05, "*", 
                                    ifelse(P < 0.1, ".", ""))))) %>%
  mutate(b = paste(z.value, Sig)) %>%
  dplyr::select(Model,Var, B, SE, b) 

reg2 <- reg1[reg1$Model == "A",colnames(reg1) != "Model"] %>% 
  full_join(reg1[reg1$Model == "B",colnames(reg1) != "Model"], by = "Var") %>% 
  full_join(reg1[reg1$Model == "C", colnames(reg1) != "Model"], by = "Var") 

rr <- bind_rows(reg2, cbind(R2_PEER, R2_Cont, R2_dem))
write.table(rr, './Data/Regressie.txt', sep = ";")
rr





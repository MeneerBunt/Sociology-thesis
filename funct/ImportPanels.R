PVladen <- function(New = F, files = './Data/PV_trace'){
  
  if (New){
    source('./funct/Basics.R')
    source('./funct/Koppel_ExtraData.R')
    ipak(c("RJDBC", "rgdal", "sf", "dplyr", "ncf", "parallel"))
    
    WW <- read.table(file = './Pass/psw.csv', sep = ";", skip = 1)
    du <- as.character(WW$V1) #getPass::getPass(msg = 'user dms')
    pw <- as.character(WW$V2) #getPass::getPass(msg = 'pw dms')
    
    ojdbc_Plek <- "C:/EigenData/Software/sqldeveloper-17.4.0.355.2349-x64/sqldeveloper/jdbc/lib/ojdbc8.jar"
    script <- './funct/ImportPanels_ADWH.sql'
    
    # opzetten connectie
    driver <- RJDBC::JDBC("oracle.jdbc.driver.OracleDriver",ojdbc_Plek,identifier.quote="'")
    conn <- dbConnect(driver, "jdbc:oracle:thin:@stdcap-orad-001:1521:psync2", du , pw)
    
    # importeer verbruikers
    print("Nieuwe data inlezen")
    print("   DMS dowload")
    
    PV <- dbGetQuery(conn, statement = readChar(script, file.info(script)$size))
    PV <- PV[!is.na(PV$X) & !is.na(PV$Y),]
    
    ## data aanpassen
    PV <- PV[PV$X != 109310 & PV$Y != 451397,] %>% mutate(PV = ifelse(DUURZAME_ENERGIE == "ZON", 1, 0)) %>%# pv omzetten naar binaire warde
      mutate(PV = ifelse(is.na(PV), 0, 1)) %>%
      group_by(STRAATNAAM, HUISNUMMER, HUISNUMMER_TOEVOEGING, POSTCODE, WOONPLAATS,
                GEMEENTE, X, Y) %>%
      summarise(PV = max(PV, na.rm = T),
                BOUWJAAR = max(BOUWJAAR, na.rm = T)) 
    
    colnames(PV)[colnames(PV) == "BOUWJAAR"] <- "BOUWJAAR_PV"
    
    print("   Berken autocorrelatie")
    Bereken_autocorrelatie(PV)
    
    save(PV, file = './Data/PV.R')
    
    ############################################################
    load(file = './Data/PV.R')
    
    print("   Bouwjaar koppelen")
    PV <- Koppel_BAGgegevens(PV) 
    
    print("   PV afstand koppelen")
    pnt <- Bereken_PVDist(PV, straal = 150,  PC_num = 5, folder = files, sub = F)
    
    print("   Inkomen koppelen")
    pnt <- Koppel_inkomen(pnt)
    
    print("   Buurtgegevens koppelen")
    pnt <- Koppel_Buurtgegevens(pnt)
     
    print("   Verkiezing uitslag koppelen")
    #pnt <- Koppel_Verkiezing(pnt) 
    
    print("   Verhuis gegevens koppelen")
    pnt <- Koppel_Verhuis(pnt)
    
    print("   SES gegevens koppelen")
    pnt <- Koppel_SES(pnt)
    
    ############################################################
   
    save(pnt, file = './Data/AanlsuitingPV_JaNee.Rda')
    
  } else {  load('./Data/AanlsuitingPV_JaNee.Rda') }
  
  return(pnt)
}

    
      
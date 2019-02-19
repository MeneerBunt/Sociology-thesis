ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(c("RJDBC", "rgdal", "sf"))

source('./Functies/Koppejan.R')
source('./Functies/ImportData_Functies.R') 

## coordinaat systeem
epsg <- make_EPSG()
amsf <- epsg$prj4[epsg$note == "# Amersfoort / RD New"]


#require(getPass)
WW <- read.table(file = './Pass/psw.csv', sep = ";", skip = 1)
du <- as.character(WW$V1) #getPass::getPass(msg = 'user dms')
pw <- as.character(WW$V2) #getPass::getPass(msg = 'pw dms')

ojdbc_Plek <- "C:/EigenData/Software/sqldeveloper-17.4.0.355.2349-x64/sqldeveloper/jdbc/lib/ojdbc8.jar"
script <- './funct/ImportPanels.sql'

# opzetten connectie
driver <- RJDBC::JDBC("oracle.jdbc.driver.OracleDriver",ojdbc_Plek,identifier.quote="'")
conn <- dbConnect(driver, "jdbc:oracle:thin:@stdcap-orad-001:1521:psync2", du , pw)

# importeer verbruikers
print("import PV")

PV <- dbGetQuery(conn, statement = readChar(script, file.info(script)$size))
PV <- PV[!is.na(PV$X) & !is.na(PV$Y),]

pnt <- SpatialPointsDataFrame(PV[,c("X", "Y")], PV, proj4string = CRS(epsg$prj4[epsg$note == "# Amersfoort / RD New"]))

mapview(pnt)

save(Aansluitingen, file = './Data/tmp/Aanlsuiting.Rda')
  
# (1) Load packages
# -----------------------------------
library(RODBC)
library(tidyverse)
library(data.table)

# (2) Close all connections
# -----------------------------------
odbcCloseAll()

# (3) Set up connection to Database
# -----------------------------------
myconn <- odbcDriverConnect(connection = paste('driver={SQL Server}',
                                               'server=mq02\\b',
                                               'database=P0515_IFFS_Segregeringens_dynamik',
                                               'trusted_connection=true', sep=';'))

# (4) Define 
#       (i) The table that you want
#           to fetch data from.
#
#       (ii) The variables that you
#            want from that table.
# -----------------------------------

# Table
LISA_tb = paste0("LISA",2001:2003,"_Individ")
background_tb = "Bakgrundsdata"
geo_tb = paste0("Geo",2001:2003)
muni_tb = paste0("la",2001:2003,".txt")

# Column/Variable names
lisa.names = c("PersonLopNr","Sun2000niva","FamStF",
                            "SocBidrpersF","DispInk",'SyssStat',"FamTypF","Civil","ArbLos",
                            "ForPeng","ForLed","SjukPP","BostBidrPersF","KapInk","Barn0_3","Barn4_6",
                            "Barn7_10","Barn11_15","Barn16_17","Lan","Kommun",
               "ArbSokNov","AlosDag","AK14Dag","Ssyk4","PeOrgLopNr","CfarLopNr","StudDelt",
               "AntFlyttTot")

back.names = c("PersonLopNr", "FodelseAr","LandKod","Kon")
geo.names = c("PersonLopNr","BostrutaX", "BostrutaY","AstrutaX","AstrutaY")


# (5) Use sqlQuery function to fetch
#     your data. (Just run!)
# -----------------------------------
backdata = sqlQuery(myconn, paste('select ', paste(back.names, collapse = ','), 
                                   ' from ', background_tb, sep = ''))
lisadata = list()
geodata = list()

for (i in 1:length(LISA_tb)) {
      lisadata[[i]] =  sqlQuery(myconn, paste('select ', paste(lisa.names, collapse = ','), 
                                     ' from ', LISA_tb[i], sep = ''))
  geodata[[i]] = sqlQuery(myconn, paste('select ', paste(geo.names, collapse = ','), 
                                    ' from ', geo_tb[i], sep = ''))
  
Final_data = Reduce(function(x,y) merge(x,y,by="PersonLopNr",all=F),
                    list(backdata,lisadata[[i]],geodata[[i]]))

Final_data = Final_data %>% subset(FodelseAr == 1983|FodelseAr == 1982|FodelseAr == 1981|FodelseAr == 1976|FodelseAr == 1979|FodelseAr == 1980|
                                     FodelseAr == 1977|FodelseAr == 1978|FodelseAr == 1975) %>% 
 as_tibble()

Final_data = Final_data %>% 
  subset(BostrutaX != 0)%>%
  subset(BostrutaX != 50) %>% as_tibble()



  setwd("//micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/municipality/")
  df_muni=read.table(muni_tb[i])
  df_muni= df_muni[-1,]
  colnames(df_muni) = c("LM","Kommun","Type_c")
  df_muni$Kommun=as.numeric(df_muni$Kommun)
  Final_data = merge(Final_data,df_muni, by = "Kommun")
  names(Final_data)
  Final_data = Final_data[,c(1,2,33,34,35,36,3:32,37,38)]

# (6) Export data? (.csv)
# -----------------------------------

# Define folder path
folder.path <- "//micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/"
incomes = paste0("income",2001:2003)
# Export (just run!)
write.csv(x = Final_data, 
          file = paste(folder.path, incomes[i],'.csv', sep=''), 
          row.names = FALSE)
}
# (7) Close connections:
# -----------------------------------
odbcCloseAll()




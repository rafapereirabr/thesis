


        
# This script downloads open data of schools and hospitals locations in Rio

# Lap top
setwd("R:/Dropbox/Dout/Data Dout")
      
      


##################### Load packages -------------------------------------------------

source("./R scripts/00_LoadPackages.R")
      
    
      



###### 1: Download Schools and Hospitals in Rio municipality----------------------
# Downloaded on 22 Jan 2016

      
  # FAZER DOWNLOAD NA MAO
  # Schools
    download.file("http://dadosabertos.rio.rj.gov.br/apiEducacao/apresentacao/csv/escolas__.csv", 
                  "./Rio places/escolas/escolas_muni.csv", quiet = FALSE)
    
  # Hospitals location
    # download.file("http://dadosabertos.rio.rj.gov.br/apiSaude/apresentacao/csv/estabelecimentos_.csv", 
    #               "./Rio places/hospitais/estabs_saudeXXX.csv", quiet = FALSE)

    temp <- fread("http://dadosabertos.rio.rj.gov.br/apiSaude/apresentacao/csv/estabelecimentos_.csv")
    fwrite(temp, "./Rio places/hospitais/estabs_saude.csv")
    
    

        
    # Hospitals Hierarchy
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/STRJ1506.dbc", 
                  "./Rio places/hospitais/hospitals_hierarchy_STRJ1506.dbc", quiet = FALSE)
    
  


        

###### 2: HOSPITALS Data Cleaning ----------------------

# read data hospitals = City data (prefeitura)
  pref <- fread("./Rio places/hospitais/estabs_saude.csv")

    
#    # Recalcula lat log com sistema Galileo do Ipea
#   # seleciona key variables
#   pref[, muni := "Rio de Janeiro"][, uf := "Rio de Janeiro"]
#   estabs_saude_latlong <- pref[, .(CNES, uf, muni, Logradouro, N?mero, CEP )]
#   names(estabs_saude_latlong)[5] <- "numero"
#   fwrite(estabs_saude_latlong,  sep = ";", file="./Rio places/hospitais/estabs_saude_latlong.csv")
#   # envia para Vanessa Na dalin Geo-localizar hospitais
#   
#   
#   pref_ipea <- fread("./Rio places/hospitais/estabs_saude_latlong_vanessaNadalin.csv", dec=",")
#   table(pref_ipea$PrecisionDepth)
#   pref_ipea[, Latitude:=as.numeric(Latitude)][, Longitude:=as.numeric(Longitude)][, CNES:=as.character(CNES)]

  
  
  
  # convert Lat Long to numeric and CNES to characther
    pref[, Latitude:=as.numeric(Latitude)][, Longitude:=as.numeric(Longitude)][, CNES:=as.character(CNES)]
    
  # change column name da variavel de nivel de ensino
    colnames(pref)[3] <- "rz_social"
    colnames(pref)[4] <- "nome"
    colnames(pref)[23] <- "ds_tipo_unidade"
    
    


   
    
# Read SUS data with classification of hospitals hierarchy
    sus <- read.dbc::read.dbc("./Rio places/hospitais/STRJ1506.dbc") # junho 2015
    setDT(sus)[, CNES := as.character(CNES) ] #  CNES to characther
    sus <- sus[, .(CNES, CODUFMUN, NIV_HIER, VINC_SUS, ATENDAMB, ATENDHOS) ] # keep columns we'll use
    
    # vars to keep?
      # CNES Unique id of health units
      # NIV_HIER - Hierarquical level of health units
      # VINC_SUS - Link to publch health System | 0 Nao 1 sim
      # ATENDHOS
      # ATENDAMB
      # ATEND_PR
    summary(sus$NIV_HIER)
    table(sus$NIV_HIER)
    

    
        
# MERGE SUS and Prefeitura data to add info on Hierarchy  
  hospt <- left_join(pref, sus, by=c("CNES"="CNES")) %>% setDT()
  
  
  # Filter 1: hospitais q atendem pelo SUS
  hospitals_filtered <- hospt[ VINC_SUS==1, ]
  rm(pref, sus, hospt)
  gc(reset = T)
  

  
  # Filter 2: types of hospitals
  hospital_types <- c(  "CENTRO DE PARTO NORMAL - ISOLADO"
                        , "CENTRO DE SAUDE/UNIDADE BASICA"
                        , "HOSPITAL ESPECIALIZADO"
                        , "HOSPITAL GERAL"
                        , "HOSPITAL/DIA - ISOLADO"
                        , "POLICLINICA"
                        , "POSTO DE SAUDE"
                        , "PRONTO ATENDIMENTO"
                        , "PRONTO SOCORRO ESPECIALIZADO"
                        , "PRONTO SOCORRO GERAL"
                        , "UNIDADE MISTA"
                        , "CLINICA/CENTRO DE ESPECIALIDADE"
                        , "CONSULTORIO ISOLADO"
                        )
  
  hospitals_filtered <- hospitals_filtered[ ds_tipo_unidade %in% hospital_types, .(CNES, Latitude, Longitude, nome, ds_tipo_unidade, NIV_HIER, VINC_SUS)]
  
  

  
  
  # Filter 3: hospitals within in the Municipality of Rio
  
      # Read shape of city
      muni <- readOGR(dsn="./Shapes_IBGE", layer="muni")      #read shape file
      muni <- spTransform(muni, CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) # lat long projection
      
      # convert Hospitals into spatial point data frame
      coordinates(hospitals_filtered) = ~Longitude + Latitude
      plot(hospitals_filtered)
      plot(muni, col="red", add=TRUE)
      
      # Apply same projection
      proj4string(hospitals_filtered) = CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      proj4string(muni) == proj4string(hospitals_filtered) # must be TRUE
      
      # intersec com Muni
      hospitals_in_muni <- point.in.poly(hospitals_filtered, muni) 
      plot(muni) ; plot(hospitals_in_muni, add=TRUE)
      
      # convert into data table
      hospitals_filtered <- setDT(as.data.frame(hospitals_in_muni))
      hospitals_filtered <- hospitals_filtered[ , .(CNES, Latitude, Longitude, nome, ds_tipo_unidade, NIV_HIER, VINC_SUS)]
      
  
      
      # Filter 4: Delete prison hospitals, etc. \\\ Elimina hospitais penintenciarios e outros (ver no arquivo ".\Data Dout\Rio places\hospitais\razo social de 337.xlsx" )
      NOT_healthcare_facilities <- c(  "CASA DE SAUDE NOSSA SENHORA DAS GRACAS"
                                       , "CENTRO DE ESTUDOS DE SAUDE DO TRABALHADOR E ECOLOGIA HUMANA"
                                       , "IPEC - FIOCRUZ"
                                       , "SEAP CGSP RJ AMB INST PENAL OSCAR STEVENSON"
                                       , "SEAP CGSP RJ AMB PENITENCIARIA TALAVERA BRUCE"
                                       , "SEAP CGSP RJ AMB PRESIDIO ARY FRANCO"
                                       , "SEAP CGSP RJ AMB PRESIDIO EVARISTO DE MORAES"
                                       , "SEAP CGSP RJ AMB PRESIDIO HELIO GOMES"
                                       , "SEAP CGSP RJ HOSP DE CUST E TRAT PSIQUIAT HEITOR CARRILHO"
                                       , "SEAP CGSP RJ SANATORIO PENAL"
                                       , "SES RJ IECAC - INST EST DE CARDIOLOGIA ALOYSIO DE CASTRO"
                                       , "SINDICATO DOS ESTIVADORES - AMBULAT"
                                       , "SMSDC NUC DE SAUDE DO TRABAL NUSAT 1"
                                       , "SMSDC NUC DE SAUDE DO TRABAL NUSAT 2"
                                       , "SMSDC PADI MIGUEL COUTO"
                                       , "SMSDC PADI SALGADO FILHO"
                                       , "SMSDC RIO PADI FRANCISCO DA SILVA TELLES"
                                       , "SMSDC RIO PADI LOURENCO JORGE"
                                       , "SMSDC RIO PADI PEDRO II"
                                       , "SPB/BRASIL"
                                       , "SSP HOSPITAL CENTRAL DA POLICIA MILITAR - HCPM"
                                       , "SMS CMS MANOEL ARTHUR VILLABOIM - AP 10" # ilha de Paqueta
                                       )
      
      hospitals_filtered <- hospitals_filtered[ !(nome %in% NOT_healthcare_facilities), ]
      

  # Filter 5: Ignore hospitals with Missing information about hierarchy
    hospitals_filtered <- hospitals_filtered[!is.na(NIV_HIER),]

    
    
    
          
# reclassify Hospitals Hierarchy -----
    # based on PDF (Manual t?cnico do cadastro nacional de estabelecimentos de sa?de - vers?o 2) p. 131
  hospitals_filtered[ , hierarq := ifelse(NIV_HIER =="01", "Low"
                                  ,ifelse(NIV_HIER =="02", "Medium"
                                  ,ifelse(NIV_HIER =="03", "Medium"
                                  ,ifelse(NIV_HIER =="04", "High"
                                  ,ifelse(NIV_HIER =="05", "Low + Medium"
                                  ,ifelse(NIV_HIER =="06", "Medium"
                                  ,ifelse(NIV_HIER =="07", "Medium + High"
                                  ,ifelse(NIV_HIER =="08", "High", NA))))))))]                   
                    
  
    # Reve hieraq com base no Tipo de Estabelecimento
    # http://tabnet.datasus.gov.br/cgi/cnes/tipo_estabelecimento.htm
    hospitals_filtered[ , hierarq := ifelse(ds_tipo_unidade == "CENTRO DE SAUDE/UNIDADE BASICA", "Low",
                                     ifelse(ds_tipo_unidade == "HOSPITAL GERAL", "Low + Medium + High",
                                     ifelse(ds_tipo_unidade == "HOSPITAL/DIA - ISOLADO", "Medium",
                                     ifelse(ds_tipo_unidade == "PRONTO ATENDIMENTO" & hierarq =="Medium", "Low + Medium",
                                     ifelse(ds_tipo_unidade == "PRONTO ATENDIMENTO" & hierarq =="High", "Low + High", hierarq))))) ] 
                                            
                                            


  
# convert Hospital hierarchy into different columns
  hospitals_filtered[ , hosp_low := ifelse( grepl("Low", hierarq), 1, 0) ]
  hospitals_filtered[ , hosp_med := ifelse( grepl("Medium", hierarq), 1, 0) ]
  hospitals_filtered[ , hosp_high := ifelse( grepl("High", hierarq), 1, 0) ]
  
  table(hospitals_filtered$hosp_high) #  92
  table(hospitals_filtered$hosp_med)  #  93
  table(hospitals_filtered$hosp_low)  # 224
  

  # hospitals_filtered[ , hosp_low := ifelse(hierarq == "Low" | hierarq == "Medium", 1, 0) ] 
  # hospitals_filtered[ , hosp_med := ifelse(hierarq == "Medium" | hierarq == "High", 1, 0) ] 
  # hospitals_filtered[ , hosp_high := ifelse(hierarq == "High", 1, 0) ]
  # High    Low Medium 
  #   86     70    153
  
  # TOTAL OF 304 hospitals 
  

  
  
  # Salve arquivo de  hospitais
  write_csv(hospitals_filtered, "./Rio places/hospitals_filtered2.csv")
  
  
  
  
  # CHECAGEM de lat log com sistema Galileo do Ipea
  
  # carrega dados com localizacao da prefeitura
  hospitals_filtered <- fread("./Rio places/hospitals_filtered2.csv")
  hospitals_filtered[, CNES:=as.character(CNES)]
  
  # carrega base com localizacao Ipea/Galileu
  pref_ipea <- fread("./Rio places/hospitais/estabs_saude_latlong_vanessaNadalin.csv", dec=",")
  table(pref_ipea$PrecisionDepth)
  pref_ipea[, lat:=as.numeric(Latitude)][, long:=as.numeric(Longitude)][, CNES:=as.character(CNES)]
  pref_ipea[, Latitude:= NULL][, Longitude:= NULL]
  
  
  
  # merge as duas bases
  a <- left_join(hospitals_filtered, pref_ipea, by="CNES") %>% setDT()
  a[, long_erro := (Longitude - long)]
  a[, lat_erro := (Latitude - lat)]
  
  # calcula distancia euclidiana (Km) entre pontos
  setDT(a)[ , errokm := distGeo(matrix(c(Longitude, Latitude), ncol = 2), 
                                matrix(c(long, lat), ncol = 2))/1000] 
  summary(a$errokm)
  
  # checa discrepancias
  ggplot(data=a) + geom_point(aes(x=Longitude, y=long, color= ifelse( errokm > 0.9 , "red","black")))
  ggplot(data=a) + geom_point(aes(x=Latitude, y=lat, color= ifelse( errokm > 0.9  , "red","black")))
  
  # seleciona apenas casos em que lat ou long diferem em mais de   1 KM
  erros <- a[ errokm > 0.9  ]
  # salva em excel
  xlsx::write.xlsx(erros, "./Rio places/hospitais/erros_hospitais.xlsx")
  
  ### confere manualmente a localizacao de cada hospital
  ### confere manualmente a localizacao de cada hospital
  ### confere manualmente a localizacao de cada hospital
  
  # le arquivo com localizacoes corrigidas 
  a <-  readxl::read_excel("./Rio places/hospitais/erros_loc_hospitais.xlsx", sheet= "erros") %>% setDT()
  a[, CNES:=as.character(CNES)]
  
  # cria colunas com lat long corretas
  a[, lat_new := ifelse( melhor_posicao == "new", lat_new, 
                         ifelse( melhor_posicao == "galileo", lat,
                                 ifelse( melhor_posicao == "muni", Latitude, NA)))]
  
  a[, long_new := ifelse( melhor_posicao == "new", long_new, 
                          ifelse( melhor_posicao == "galileo", long,
                                  ifelse( melhor_posicao == "muni", Longitude, NA)))]
  # keep selected columns
  a <- a[, .(CNES, lat_new, long_new)]
  names(a)[2:3] <- c('Latitude', 'Longitude')
  
  # update Lat Long with correct values ||| http://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc
  dat2 <- merge(hospitals_filtered, a, by = "CNES", all.x = TRUE) %>% setDT() # merge data
  dat2[, Latitude := ifelse(is.na(Latitude.y), Latitude.x, Latitude.y)] # correct Lat
  dat2[, Longitude := ifelse(is.na(Longitude.y), Longitude.x, Longitude.y)] # correct long
  dat2[, Latitude.y := NULL][, Latitude.x := NULL][, Longitude.y := NULL][, Longitude.x := NULL] # drop old lat long
  
  
  # Correct location of selected hospitals
  dat2[ nome == "UFRJ INST DE PUER PED MARTAGAO GESTEIRA", c('Longitude', 'Latitude') := .( -43.23903,-22.84116 ) ]
  
  
  hospitals_filtered[ nome %like% "UNIVERSI"]
  hospitals_filtered[ nome %like% "UFRJ"]
  
  
  # Save Hospitals with correct location
  fwrite(dat2, "./Rio places/hospitals_filtered_correct.csv")
  dat2 <- fread("./Rio places/hospitals_filtered_correct.csv")
  table(dat2$hierarq)
  
  # check plot
  dat2$Longitude <- as.numeric(dat2$Longitude)
  dat2$Latitude <- as.numeric(dat2$Latitude)
  
  coordinates(dat2) = ~Longitude + Latitude
  plot(muni, col="red")
  plot(dat2, add=TRUE)
  
  
  
###### 3: SCHOOLS Data Cleaning ----------------------
  
  # rodar codigo para salvar dados no INEP
  source("./Rio places/escolas_inepipea/inep_latlong.R")
  
  # read data
  schools <- fread("./Rio places/escolas_inepipea/escolas_rio_2015.csv")
  
  
  
  
  ### DATA CLEAN SCHOOLS
  
  
  # remove escolas penitenciarias
  schools <- schools[ IN_LOCAL_FUNC_PRISIONAL_SOCIO == "Não", ]
  
  # remove escolas privadas
  table(schools$REDE)
  schools <- schools[ REDE != "Privada", ]
  
  # total number of employees
  summary(schools$NU_FUNCIONARIOS)
  
  sum(schools$NU_FUNCIONARIOS)
  # 86527
  ggplot() + geom_point(data= schools, aes(x=lon,y=lat, size=NU_FUNCIONARIOS)) +coord_equal()
  
  
  # Keep only secondary shools (nivel medio e Educacao profisional)
    table(schools$esc)
  
  table(schools$ESC_MED)
  table(schools$ESC_PROF)
  schools <- subset(schools, ESC_PROF ==1 | ESC_MED==1)



  # total 279  schools
    schools_filtered <- schools[complete.cases(lat), ] # remove NAs
    schools_filtered <- schools[complete.cases(lon), ] # remove NAs
    

  # Save schools file
    fwrite(schools_filtered, file="./Rio places/schools_filtered.csv")


ggplot() + geom_point(data=schools_filtered, aes(x=lon,y=lat))
   
    
    
    
    
     
    
# Analisa dados de hospitais obitidos via shape file no site da prefeitura
      #     # Escolas publics ? Nao dah para saber
      #     schoolsshp <- readOGR(dsn = './Rio places/escolas', layer ='Escolas_Municipais')
      #     schoolsshp.df <- setDT(as.data.frame(schoolsshp))
      #     schoolsshp.df[Flg_Gestao==0, link_carte]
    
    
    
    
    
###### 2: Plot Maps ----------------------
  

# Plot Schools
  ggplot() +
    geom_point(data=schools, aes(x=Longitude, y=Latitude)) +
    coord_map(xlim = c(-43.828854, -43.117908),ylim = c(-23.091709, -22.760493))
    
# Plot Hospitals

  ggplot(data=hospitals_filtered) +
    geom_point(aes(x=Longitude, y=Latitude, color=hierarq),  alpha=0.7) +
    coord_map(xlim = c(-43.828854, -43.117908),ylim = c(-23.091709, -22.760493))

  ggplot() +
    geom_point(data=subset(hospitals_filtered, hosp_low==1), aes(x=Longitude, y=Latitude, text=paste("CNES",CNES)), color="blue" , alpha=0.7) +
    geom_point(data=subset(hospitals_filtered, hosp_med==1), aes(x=Longitude, y=Latitude, text=paste("CNES",CNES)), color="green",  alpha=0.7) +
    geom_point(data=subset(hospitals_filtered, hosp_high==1), aes(x=Longitude, y=Latitude, text=paste("CNES",CNES)), color="red",  alpha=0.7) +
    coord_map(xlim = c(-43.828854, -43.117908),ylim = c(-23.091709, -22.760493))
  
  ggplotly()
  
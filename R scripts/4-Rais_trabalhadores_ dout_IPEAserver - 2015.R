
# Leitura e espacializacao dos Dados da Rais Identificada
# julho, 2015. Brasilia, Brasil
# por Rafael Pereira


# Esse Script:

          # 1 Abre Rais Identificada do Servidor do Ipea
          # 2 Filtra apenas empregados no Rio de Janeiro
          # 3 Limpa casos de empregos publicos cuja localizacao esta errada
start from 4 so you do not need to read all data
          # 4 Seleciona apenas variaveis de interesse
          # 5 Recodifica variaveis
          # 6 Merge Base trabalhadores e Empresas

# 7 Impute Education data to Grid data base
          # Espacializa empregos por CEP



### 0: Set Working Directory ------------

# Ipea Server
setwd("D:/Users/R1701707/Desktop/Dout/Data Dout")




##################### Load packages -------------------------------------------------------

source("./R scripts/00_LoadPackages.R")





# # 1 Load Rais Trabalhadores do Servidor do Ipea ----------
# 
#   raisbrasil2015 <- "//STORAGE2/bases/RESTRITO/RAIS IDENTIFICADA/FormatoSTATA/brasil2015.dta"
#   system.time(raisbrasil2015 <- read.dta(raisbrasil2015)) # 73.326.485 observations, in 426.6 minutos
#   
#   #   raisbrasil2015 <- "//STORAGE2/bases/RESTRITO/RAIS IDENTIFICADA/FormatoSAS/estab2015.sas7bdat"
#   #   sectionnames_doc <- "//STORAGE2/bases/RESTRITO/RAIS IDENTIFICADA/versao Serpro/Programas/Programa de importa??o dos dados Serpro de 2013 e 2014.sas"
#   # 
#   #     system.time(raisbrasil2015 <- read.ssd(raisbrasil2015, sectionnames= sectionnames_doc))
#   
#   # Convert to Data.Table to make fast operations
#   raisbrasil2015 <- as.data.table(raisbrasil2015)
  
  
# 1 Load Rais Trabalhadores do PC (enviado pela Van) ----------
    
  rais2015trab_rj <- fread("./rais/rais2015trab_rj.csv", colClasses=list(character=1:108))
  head(rais2015trab_rj)
  
  # Select  21 out of 108 variables
  rais2015trab_rj <- rais2015trab_rj[, .(cpf, pis, ind_cpf_val, ind_pis_val, nome_trab, id_estab, idade, nome_trab, cnpj_raiz, cei_vinc, codemun, mun_trab, genero, grau_instr, cbo2002, clas_cnae10, clas_cnae20, rem_dez_r, rem_med_r, salario, horas_contr)] # nat_jur2009
  gc(reset = T)
  
  
  # convert coordinates and number of jobs (qtd_vinc_ativos) to numeric 
  rais2015trab_rj[, cnpj_raiz := as.numeric(cnpj_raiz)][, id_estab := as.numeric(id_estab)][, cei_vinc := as.numeric(cei_vinc)]
  head(rais2015trab_rj)
  gc(reset = T)
  
  # create unique company ID
  rais2015trab_rj[, unique_company_id := paste(cnpj_raiz, id_estab,  cei_vinc, sep = "-")] # 00000000
  head(rais2015trab_rj)
  
  

  
# 2 Filtra apenas empregados no Rio de Janeiro ----------
  
  head(rais2015trab_rj$codemun) # check number of digits
  
  #Filtra apenas Rio, embora Municipio do Rio seja "3304557"
  rais2015trab_rj <- subset(rais2015trab_rj, codemun == "330455") # 3.785.058 observations
  gc(reset = T)
  
  # No repeated workers, remaining 3.712.681 workers
  rais2015trab_rj <- unique(rais2015trab_rj)
  

  

#Variaveis de interesse
    # id_estab    - identificador CNPJ
    # cnpj_raiz   - CNPJ da Empresa
    # cei_vinc    - CEI vinculado - Cadastro Especifico do INSS
    # CODEMUN   	-	Codigo do Municipio # Rio de Janeiro	330455
    # mun_trab	  - Munic?pio local de trabalho
    # UF        	-	Unidade da Federacao # RJ
    # xxx NAT_ESTB    - Natureza do Estabelecimento
    # NAT_JUR2009     - Natureza do Estabelecimento detalhada 2009
    # GENERO    	- sexo 
    # GRAU_INSTR	-	Grau de instrucao
    # xxx SUBS_IBGE 	-	Subsetor de atvidade (CNAE simples)
    # CBO2002   	-	Grupo Base CBO ocupacao
    # REM_DEZ_R 	-	Remuneracao em dezembro em Reais
    # REM_MED_R 	-	Remuneracao media em Reais
    # HORAS_CONTR	-	Horas contratadas
    # clas_cnae10	- Classe CNAE 1.0
    # clas_cnae20	-	Classe CNAE 2.0
    # salario	    - Sal?rio contratual (R$)
  
  
  
  
## 5 Recode variables of workers ---------------------
  

    # Recode Education Variable
      rais2015trab_rj[ grau_instr <= 6 , edu := 1]
      rais2015trab_rj[ grau_instr == 7 | grau_instr ==8, edu := 2]
      rais2015trab_rj[ grau_instr >= 9 , edu := 3]
      # "Less than high school", "High school","University degree",
      
      table(rais2015trab_rj$edu)
    

    
      
## 6 Merge basde estabelecimentos e trabalhadores   -------------------
# This will  Add Spatial Variables (Grids, lat long) to Rais Individual records
    
    # Load Companies Records
      rais2015estab_rj_grid <- fread("./rais/rais2015estab_rj_grid.csv" )
      setDT(rais2015estab_rj_grid)[, long := as.numeric(long)][, lat := as.numeric(lat)]
      

      
    # Merge the two objects
      rais2015trab_rj[, nome_trab := NULL]
      rais2015trab_rj_grid <- rais2015trab_rj[rais2015estab_rj_grid, on="unique_company_id", nomatch=0]

      
    # keep only jobs inside 200m grid , fica com 2.909.385 obs
      rais2015trab_rj_grid <- subset(rais2015trab_rj_grid, !is.na(id200))
      rais2015trab_rj_grid <- subset(rais2015trab_rj_grid, !is.na(lat))
      
      
    
# quick plot
  ggplot(rais2015trab_rj_grid, aes(x=long, y=lat)) + stat_binhex(bins=110) + coord_equal()
      
      
    # Save rais2015estab_rj_grid as csv file
      fwrite(x=rais2015trab_rj_grid, file="./rais/rais2015trab_rj_grid.csv")
      # rais2015trab_rj_grid <- fread("./rais/rais2015trab_rj_grid.csv")
      
      
      
      
## 7 Impute Education data to Grid data base ----------------------
    
  # read data with geolocated workers records
    workers_grid <- fread("./rais/rais2015trab_rj_grid.csv")       
    companies_grid <- fread("./rais/rais2015estab_rj_grid.csv")   
      

      nrow(workers_grid)
      nrow(companies_grid)
      
      
# Get edu composition of workers in education secretary
  educ_secret <- workers_grid[ razao_social == "SECRETARIA DE ESTADO DE EDUCACAO",]
  edu_composition <- table(educ_secret$edu) / length(educ_secret$edu)
    
    
nrow(educ_secret) # 89798


### Remove workers from top problematic employers -------------------
    
  # These are mostly Oursourcing firms, and public institutions with many offices but that declared all employees working from the main office.
  # I've checked the 50 largest institutions (account for 17,7% of all jobs). 
  # In total, 17 institutions were removed from the database, 
  
    # search CNPJ
    # http://empresasdobrasil.com/resultadogoogle?q=2757614002949
    
    
# identify top employers   
  top_employers <- workers_grid[, .(qtd_vinc_ativos = .N), by=.(razao_social,lat, long, unique_company_id) ][order(-qtd_vinc_ativos)] 
  top_employers <- head(top_employers, n=50)

  # quick map
  ggplot(top_employers, aes(x=long, y=lat)) + stat_binhex(bins=100) + coord_equal()
  ggplot(top_employers, aes(x=long, y=lat)) + geom_point(aes(size=qtd_vinc_ativos), alpha=.3) + coord_equal()
  
  
# check 20 maiores empregadores. Remove those with dispersed jobs (mostly public institutions and 2 terceirizadas)
  
companies_to_remove <- c("SECRETARIA DE ESTADO DE EDUCACAO"
                          , "POLICIA MILITAR DO ESTADO DO RIO DE JANEIRO"
                          , "NOVA RIO SERVICOS GERAIS LTDA"
                          , "MINISTERIO DA SAUDE"
                          , "RJ - R HANNIBAL PORTO 450"
                          , "CORPO DE BOMBEIROS MILITAR DO ESTADO DO RIO DE JANEI"
                        # , "TRIBUNAL DE JUSTICA DO ESTADO DO RIO DE JANEIRO" # ??????????????????????
                          , "SECRETARIA DE ESTADO DE SAUDE"
                          , "FUNDACAO DE APOIO A ESCOLA TECNICA DO ESTADO DO RIO"
                          , "POLICIA CIVIL DO ESTADO DO RIO DE JANEIRO"
                          , "PROL CENTRAL DE SERVICOS LTDA"
                          , "HOPE RECURSOS HUMANOS S.A."
                          , "GUARDA MUNICIPAL DA CIDADE DO RIO DE JANEIRO GM RIO"
                          , "CNS NACIONAL DE SERVICOS LIMITADA"
                          , "ANGELS SERVICOS TECNICOS LTDA"
                          , "COMPANHIA ESTADUAL DE AGUAS E ESGOTOS - CEDAE"
                          , "GAP RIO DE JANEIRO"
                          , "FUNDACAO SAUDE DO ESTADO DO RIO DE JANEIRO"
                          )
# get data of companies to remove
  companies_to_remove <- top_employers[ razao_social %in% companies_to_remove, ]
  

# quick map
  ggplot(companies_to_remove, aes(x=long, y=lat)) + stat_binhex(bins=100) + coord_equal()
  ggplot(companies_to_remove, aes(x=long, y=lat)) + geom_point(aes(size=qtd_vinc_ativos), alpha=.3) + coord_equal()
  ggmap(get_map(location = "Rio de Janeiro", zoom=10)) + geom_point(data=companies_to_remove,  aes(x=long, y=lat, size=qtd_vinc_ativos), alpha=.7, color='red') + coord_equal()

  
# % of jobs in 50 top employers 
  sum(top_employers$qtd_vinc_ativos)  / nrow(workers_grid) *100

# % of jobs in companies to remove 
# number of jobs ignored (in % of all formal jobs)
  sum(companies_to_remove$qtd_vinc_ativos) / nrow(workers_grid) *100

  
# drop workers from those companies
  workers_grid <- workers_grid[ !(razao_social %in% companies_to_remove$razao_social) ] 
    
# quick map
  ggplot(workers_grid, aes(x=long, y=lat)) + stat_binhex(bins=100) + coord_equal()


  
  
  
  
### 8 Get number of jobs by Education level in each Grid cell  ---------------------


# creat a count variable
workers_grid[, vcount := 1]

# sum number of jobs in each edu level by grid cell
grid200_edu_levels <- workers_grid[, .(edubas = sum(vcount[which(edu==1)], na.rm=T),
                                       edumed = sum(vcount[which(edu==2)], na.rm=T),
                                       edusup = sum(vcount[which(edu==3)], na.rm=T) ), by=id200]

# check if we didn't miss any job on the process. It must me TRUE to be Ok.
  sum(grid200_edu_levels$edubas, grid200_edu_levels$edumed, grid200_edu_levels$edusup, na.rm=T) == nrow(workers_grid)




### Recover workers from public schools in Censo Escolar -----------------

  # we recover 83.589 jobs in the secretary of education 
  
  
# lat long and utm projections
myprojection_latlong <-  CRS("+proj=longlat +zone=23 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# read all schools
schools <- fread("./Rio places/escolas_inepipea/escolas_rio_2015.csv")

# remove escolas privadas
schools <- schools[ REDE != "Privada", ]

# keep necessary columns
schools <- schools[, .(CO_ENTIDADE, lon, lat, NU_FUNCIONARIOS)]
schools <- na.omit(schools)


# total number of employees
summary(schools$NU_FUNCIONARIOS)
sum(schools$NU_FUNCIONARIOS) # 83604

 

# convert to spatial points 
coordinates(schools) = ~lon + lat

# Load Spatial Grid
gridIBGE200 <- readOGR(dsn="./Shapes_IBGE", layer="gridIBGE200")

# Apply same projection
proj4string(schools) = myprojection_latlong
gridIBGE200 <- spTransform(gridIBGE200, myprojection_latlong)
proj4string(gridIBGE200) == proj4string(schools) # must be TRUE
plot(schools)

# intersec com grid 200
  schools_in_grid200 <- point.in.poly(schools, gridIBGE200) 
  schools_in_grid200 <- as.data.frame(schools_in_grid200) %>% setDT()

# Apply edu composition of all public teachers to each school
  schools_in_grid200[, school_edubas := NU_FUNCIONARIOS * edu_composition[[1]] ]
  schools_in_grid200[, school_edumed := NU_FUNCIONARIOS * edu_composition[[2]] ]
  schools_in_grid200[, school_edusup := NU_FUNCIONARIOS * edu_composition[[3]] ]


# get number of public teachers by edu level in each grid cell
teachers_in_grid200 <- schools_in_grid200[, .(school_edubas=round(sum(school_edubas)),
                                              school_edumed=round(sum(school_edumed)),
                                              school_edusup=round(sum(school_edusup))), by = id200]

# total jobs of public teachers
teachers_in_grid200[, sum(school_edubas,school_edumed, school_edusup)] ### 83.589




# merge with all other jobs  
  # merge
  temp <- full_join(grid200_edu_levels, teachers_in_grid200, by="id200") %>% setDT()
  
  # sum rais jobs with public teacher jobs from censo escolar
  temp[, edubas2 := sum(edubas, school_edubas, na.rm=T), by=id200]
  temp[, edumed2 := sum(edumed, school_edumed, na.rm=T), by=id200]
  temp[, edusup2 := sum(edusup, school_edusup, na.rm=T), by=id200]

# check number of jobs after
temp[, sum(edubas2, edumed2, edusup2, na.rm=T)] ###   2.608.073 + 83.589 == 2.691.662

# keep necessary columns and rename them
temp <- temp[, .(id200, edubas2, edumed2, edusup2)]
names(temp) <- c('id200', 'edubas', 'edumed', 'edusup')

temp[, totaljobs := sum(edubas, edumed, edusup, na.rm=T), by=id200 ]


# save info of number of jobs and 
fwrite(temp, file = "./rais/grid200_edu_levels_2015.csv")



### total de empregos no final
sum(temp$totaljobs)


sum(temp$totaljobs)/ 2914238





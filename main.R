
# PRACA INŻYNIERSKA - ADRIAN NOWACKI


#  #  #  #  #  #  #  #  #  #  #  #  #  #  WCZYTANIE DANYCH I PAKIETÓW

library(dplyr)
library(sf)
library(tmap)

    # # # ROK 1990
    block_1990 = read.csv("../us_race/1990/nhgis0105_ds120_1990_block.csv")
    grp_blocks_1990 = read.csv("../us_race/1990/nhgis0105_ds120_1990_blck_grp.csv")
    tract_1990_surowe = read.csv("../us_race/1990/nhgis0105_ds120_1990_tract.csv") 
    
    # # # ROK 2000 
    block_2000 = read.csv("../us_race/2000/nhgis0106_ds147_2000_block.csv")
    grp_blocks_2000 = read.csv("../us_race/2000/nhgis0106_ds147_2000_blck_grp.csv")
    tract_2000 = read.csv("../us_race/2000/nhgis0107_ds146_2000_tract.csv")
    
    # # # ROK 2010
    block_2010 = read.csv("../us_race/2010/nhgis0105_ds172_2010_block.csv")
    grp_blocks_2010 = read.csv("../us_race/2010/nhgis0105_ds172_2010_blck_grp.csv")
    tract_2010 = read.csv("../us_race/2010/nhgis0105_ds172_2010_tract.csv")
    
    # # # ROK 2020 
    
    block_2020 = read.csv("../us_race/2020/nhgis0105_ds248_2020_block.csv")
    grp_blocks_2020 = read.csv("../us_race/2020/nhgis0105_ds248_2020_blck_grp.csv")
    tract_2020 = read.csv("../us_race/2020/nhgis0105_ds248_2020_tract.csv")
    
   
    
# zmiana nazw kolumn z "FMS" do "FYF"
# names(tract_2000)[31:39]<- paste0("FYF00", 1:9)
# names(tract_2000)[40:44]<- paste0("FYF0", 10:14)


     all_files = list(block_1990, grp_blocks_1990, tract_1990, block_2000, grp_blocks_2000, 
                      tract_2000, block_2010, grp_blocks_2010, tract_2010, block_2020, 
                      grp_blocks_2020, tract_2020)
     
    
    

#  #  #  #  #  #  #  #  #  #  #  #  #  #  REKLASYFIKACJA


reclass <- function(x) { 
  if (ncol(x) == 36){
    x = x %>% mutate(white = ET2001,
                     black = ET2002,
                     american = ET2003, 
                     asian = ET2004,
                     other = ET2005,
                     latin = ET2006 + ET2007 + ET2008 + ET2009 + ET2010)
    x <- x[, c(1:2, 14, 22, 37: ncol(x))]
  }
  else if(ncol(x) == 27){  #| ncol(x) == 44){
    x <- x %>% mutate(white = FYF001,
                      black = FYF002,
                      american = FYF003, 
                      asian = FYF004 + FYF005,
                      other = FYF006 + FYF007,
                      latin = FYF008 + FYF009 + FYF010 + FYF011 + FYF012 + FYF013 + FYF014)
    x <- x[, c(1:2, 6, 4, 28: ncol(x))]
  }
  else if(ncol(x) == 44){
    x <- x %>% mutate(white = FMS001,
                      black = FMS002,
                      american = FMS003, 
                      asian = FMS004 + FMS005,
                      other = FMS006 + FMS007,
                      latin = FMS008 + FMS009 + FMS010 + FMS011 + FMS012 + FMS013 + FMS014)
    x <- x[, c(1:2, 8, 6, 45: ncol(x))]
  }
  else if(ncol(x) == 56){
    x = x %>% mutate(white = H7Z003,
                     black = H7Z004,
                     american = H7Z005, 
                     asian = H7Z006 + H7Z007,
                     other = H7Z008 + H7Z009,
                     latin = H7Z010)
    x <- x[, c(1:2, 8, 6, 57: ncol(x))]
  }
  else if(ncol(x) == 124){
    x = x %>% mutate(white = U7C005,
                     black = U7C006,
                     american = U7C007, 
                     asian = U7C008 + U7C009,
                     other = U7C010 + U7C011,
                     latin = U7C002)
    x <- x[, c(1:2, 10, 8, 125: ncol(x))]
  }
  
}


reclassify <- function(){
  for (i in length(all_files)){
    empty <- lapply(all_files[1:i], reclass)
    block_1990 <<- as.data.frame(empty[1])
    grp_blocks_1990 <<- as.data.frame(empty[2])
    tract_1990 <<- as.data.frame(empty[3])
    block_2000 <<- as.data.frame(empty[4])
    grp_blocks_2000 <<- as.data.frame(empty[5])
    tract_2000 <<- as.data.frame(empty[6])
    block_2010 <<- as.data.frame(empty[7])
    grp_blocks_2010 <<- as.data.frame(empty[8])
    tract_2010 <<- as.data.frame(empty[9])
    block_2020 <<- as.data.frame(empty[10])
    grp_blocks_2020 <<- as.data.frame(empty[11])
    tract_2020 <<- as.data.frame(empty[12])
  }
}

reclassify()


#  #  #  #  #  #  #  #  #  #  #  #  #  #  WCZYTANIE FUNKCJI OBLICZAJĄCYCH WSKAŹNIKI

#  #  # ENTROPIA
entropy_funct = function(proc) {
  entropy = -sum(ifelse(proc > 0, proc * log(proc, base = exp(1)), 0)) # obliczenie entropii
  return(entropy)
} 

 
#  #  # ENTROPIA STANDARYZOWANA
entropy_std_funct = function(proc) {
  entropy = - sum(ifelse(proc > 0, proc * log(proc, base = exp(1)), 0)) # obliczenie entropii
  entropy_std = entropy / log(length(proc), base = exp(1))              # wykonanie standaryzacji
  return(entropy_std)
}


#  #  # WSKAŹNIK NIEPODOBIEŃSTWA D
d_index = function(x, y) {                                             # x, y - liczba osob dla 1 i 2 jednostki spisowej
  d = sum(abs(x / sum(x, na.rm=TRUE) - y / sum(y, na.rm=TRUE))) / 2
  return(d)
}


#  #  # WSKAŹNIK TEORII INFORMACJI H
h_index <- function(races) {
  races_all = apply(races, 2, sum, na.rm=TRUE)                         # liczba osob w calym obszarze w podziale na grupy rasowo-etniczne
  pop = sum(races_all, na.rm=TRUE)                                     # liczba osob dla calego obszaru
  pop_i = apply(races, 1, sum, na.rm=TRUE)                             # liczba osob dla kazdej jednostki spisowej
  proc = races / pop_i                                                 # procent osob dla danej grupy w kazdej jednostce spisowej
  proc_all = races_all / sum(races_all, na.rm = TRUE)                  # procent osob dla danej grupy dla calego obszaru
  ent_i = apply(proc, 1, entropy_funct)                                # entropia dla kazdej jednostki spisowej
  ent = entropy_funct(proc_all)                                        # entropia dla calego obszaru
  h_ind = sum(pop_i * (ent - ent_i) / (ent * pop), na.rm=TRUE)         # obliczenie wskaznika H
  return(h_ind)
}




#  #  #  #  #  #  #  #  #  #  #  #  #  # WCZYTANIE FUNKCJI AGREGUJĄCYCH DANE DLA POSZCZEGÓLNYCH WSKAŹNIKÓW


list_race <- c("white", "black", "american", "asian", "other", "latin") # lista ras

index_entropia <- function(x){
  grpd <- x %>% group_by(COUNTYA, STATEA) %>% arrange(COUNTYA)          # pogrupowanie danych wedlug kodu hrabstwa i stanu
  splt <- group_split(grpd )                                            # rozdzielenie pogrupowanych danych na ramki danych
  races <- lapply(splt, function(splt) splt[!(names(splt) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))]) # wyszczegolnienie kolumn tylko z rasami
  races_all <- lapply(races, colSums, na.rm = TRUE)                     # liczba osob dla kazdego hrabstwa w podziale na grupy rasowo-etniczne
  pop <- lapply(races_all, sum)                                         # liczba osob dla calego hrabstwa
  perc <- Map("/", races_all, pop)                                      # procent osob dla danej grupy w hrabstwie
  ent  <- lapply(perc, entropy_funct)                                   # obliczenie entropii dla kazdego hrabstwa
  
  # entropia standaryzowana
  Ent_std_index <- lapply(perc, entropy_std_funct)                      
  ent_std_table <- do.call(rbind.data.frame, Ent_std_index)            
  
  # tabela
  ent_table <- do.call(rbind.data.frame, ent)                           
  Ent_indexes <- cbind(ent_table, ent_std_table)                         
  colnames(Ent_indexes) <- c("Entropy", "Entropy_std")                  
  Ent_indexes <- round(Ent_indexes, 4)
  county_num <- group_keys(grpd)                                        # wyszczegolnienie kodu zlaczenia danych
  Ent_indexes <- cbind(county_num, Ent_indexes)                         # ramka danych z kodem stanu, hrabstwa oraz dwoma wskaznikami
}

index_H <- function(x){
  grpd <- x %>% group_by(COUNTYA, STATEA) %>% arrange(COUNTYA)          # pogrupowanie danych wedlug kodu hrabstwa i stanu
  splt <- group_split(grpd)
  races <- lapply(splt, function(splt) splt[!(names(splt) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))])
  
  h_indexes <- lapply(races, h_index)
  
  h_indexes <- do.call(rbind.data.frame, h_indexes)
  colnames(h_indexes) <- "H"
  h_indexes <- round(h_indexes, 4)                                      # ramka danych ze wskaznikiem H dla kazdego hrabstwa
}

index_D <- function(x){
  grpd <- x %>% group_by(COUNTYA, STATEA) %>% arrange(COUNTYA)          # pogrupowanie danych wedlug kodu hrabstwa i stanu
  splt <- group_split(grpd)
  races <- lapply(splt, function(splt) splt[!(names(splt) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))])
  
  white <- sapply(races, function(x) x%>% select(white))                # zapisanie poszczegolnych grup rasowo-etnicznych do obliczen do nowych obiektow
  black <- sapply(races, function(x) x%>% select(black))
  asian <- sapply(races, function(x) x%>% select(asian))
  other <- sapply(races, function(x) x%>% select(other))
  latin <- sapply(races, function(x) x%>% select(latin))
  
  D_wb <- as.data.frame(mapply(d_index, white, black))
  D_wa <- as.data.frame(mapply(d_index, white, asian))
  D_wl <- as.data.frame(mapply(d_index, white, latin))
  D_bl <- as.data.frame(mapply(d_index, black, latin))
  D_ba <- as.data.frame(mapply(d_index, black, asian))
  D_la <- as.data.frame(mapply(d_index, latin, asian))
  
  D_indexes <- cbind(D_wb, D_wa, D_wl, D_bl, D_ba, D_la)
  colnames(D_indexes) <- c("D_wb", "D_wa", "D_wl", "D_bl", "D_ba", "D_la")
  D_indexes <- round(D_indexes, 4)                                      # ramka danych ze wskaznikami D dla kazdego hrabstwa
}



#  #  #  #  #  #  #  #  #  #  #  #  #  # OBLICZENIE WSKAŹNIKÓW DLA KAŻDEGO PLIKU


all_files <- list(block_1990, block_2000, block_2010, block_2020, grp_blocks_1990, grp_blocks_2000, 
                  grp_blocks_2010, grp_blocks_2000, tract_1990, tract_2000, tract_2010, tract_2000)


indexes <- function(){
  for (i in length(all_files)){
    ent_list <- lapply(all_files[1:i], index_entropia)                   # utworzenie list z ramkami danych poszczegolnych wskaznikow dla kazdego hrabstwa
    H_list <- lapply(all_files[1:i], index_H)
    D_list <- lapply(all_files[1:i], index_D)
    list<- mapply(function(a, b, c) {                                    # polaczenie kazdej z trzech ramek danych ze wskaznikami w jedna ramke
      binded <- cbind(a, b, c)
    }, ent_list, H_list, D_list, SIMPLIFY = FALSE)
    
    block_1990 <<- as.data.frame(list[1])                                # przypisanie ramek danych ze wskaznikami do pierwotnych plikow
    block_2000 <<- as.data.frame(list[2])
    block_2010 <<- as.data.frame(list[3])
    block_2020<<- as.data.frame(list[4])
    grp_blocks_1990 <<- as.data.frame(list[5])
    grp_blocks_2000 <<- as.data.frame(list[6])
    grp_blocks_2010 <<- as.data.frame(list[7])
    grp_blocks_2020 <<- as.data.frame(list[8])
    tract_1990 <<- as.data.frame(list[9])
    tract_2000 <<- as.data.frame(list[10])
    tract_2010 <<- as.data.frame(list[11])
    tract_2020 <<- as.data.frame(list[12])
  }
}
indexes()       

# write.csv(tract_1990, "counties_csv/tract_1990.csv")
# write.csv(tract_2000, "counties_csv/tract_2000.csv")
# write.csv(tract_2010, "counties_csv/tract_2010.csv")
# write.csv(tract_2020, "counties_csv/tract_2020.csv")



#  #  #  #  #  #  #  #  #  #  #  #  #  #  WCZYTANIE GOTOWYCH DANYCH

# # # ROK 1990
block_1990 = read.csv("counties_csv/block_1990.csv")
grp_blocks_1990 = read.csv("counties_csv/grp_blocks_1990.csv")
tract_1990 = read.csv("counties_csv/tract_1990.csv") 

# # # ROK 2000 
block_2000 = read.csv("counties_csv/block_2000.csv")
grp_blocks_2000 = read.csv("counties_csv/grp_blocks_2000.csv")
tract_2000 = read.csv("counties_csv/tract_2000.csv")

# # # ROK 2010
block_2010 = read.csv("counties_csv/block_2010.csv")
grp_blocks_2010 = read.csv("counties_csv/grp_blocks_2010.csv")
tract_2010 = read.csv("counties_csv/tract_2010.csv")

# # # ROK 2020 
block_2020 = read.csv("counties_csv/block_2020.csv")
grp_blocks_2020 = read.csv("counties_csv/grp_blocks_2020.csv")
tract_2020 = read.csv("counties_csv/tract_2020.csv")

# # # usuniecie zbędnej kolumny
x_column <- function(){
  block_1990 <<- block_1990[, -1]
  block_2000 <<- block_2000[, -1]
  block_2010 <<- block_2010[, -1]
  block_2020 <<- block_2020[, -1]
  grp_blocks_1990 <<- grp_blocks_1990[, -1]
  grp_blocks_2000 <<- grp_blocks_2000[, -1]
  grp_blocks_2010 <<- grp_blocks_2010[, -1]
  grp_blocks_2020 <<- grp_blocks_2020[, -1]
  tract_1990 <<- tract_1990[, -1]
  tract_2000 <<- tract_2000[, -1]
  tract_2010 <<- tract_2010[, -1]
  tract_2020 <<- tract_2020[, -1]
}
x_column()
# # # # # rozdzielenie plikow na wskazniki i lata
rozdzielenie <- function(){
    # # # # wskaznik H
    ind_H_2020 <<- data.frame("H_block" = block_2020$H,
                              "H_group_blocks" = grp_blocks_2020$H,
                              "H_tract" = tract_2020$H)
    
    ind_H_2010 <<- data.frame("H_block" = block_2010$H,
                              "H_group_blocks" = grp_blocks_2010$H,
                              "H_tract" = tract_2010$H)
    
    ind_H_2000 <<- data.frame("H_block" = block_2000$H,
                              "H_group_blocks" = grp_blocks_2000$H,
                              "H_tract" = tract_2000$H)
     
    ind_H_1990 <<- data.frame("H_block" = block_1990$H,
                              "H_group_blocks" = grp_blocks_1990$H,
                              "H_tract" = tract_1990$H)
    
    
    # # # # wskaznik D
    ind_D_2020 <<- data.frame("D_wb_block" = block_2020$D_wb,
                              "D_wa_block" = block_2020$D_wa,
                              "D_wl_block" = block_2020$D_wl,
                              "D_bl_block" = block_2020$D_bl,
                              "D_ba_block" = block_2020$D_ba,
                              "D_la_block" = block_2020$D_la,
                              
                              "D_wb_group_blocks" = grp_blocks_2020$D_wb,
                              "D_wa_group_blocks" = grp_blocks_2020$D_wa,
                              "D_wl_group_blocks" = grp_blocks_2020$D_wl,
                              "D_bl_group_blocks" = grp_blocks_2020$D_bl,
                              "D_ba_group_blocks" = grp_blocks_2020$D_ba,
                              "D_la_group_blocks" = grp_blocks_2020$D_la,
                              
                              "D_wb_tract" = tract_2020$D_wb,
                              "D_wa_tract" = tract_2020$D_wa,
                              "D_wl_tract" = tract_2020$D_wl,
                              "D_bl_tract" = tract_2020$D_bl,
                              "D_ba_tract" = tract_2020$D_ba,
                              "D_la_tract" = tract_2020$D_la)
    
    ind_D_2010 <<- data.frame("D_wb_block" = block_2010$D_wb,
                              "D_wa_block" = block_2010$D_wa,
                              "D_wl_block" = block_2010$D_wl,
                              "D_bl_block" = block_2010$D_bl,
                              "D_ba_block" = block_2010$D_ba,
                              "D_la_block" = block_2010$D_la,
                              
                              "D_wb_group_blocks" = grp_blocks_2010$D_wb,
                              "D_wa_group_blocks" = grp_blocks_2010$D_wa,
                              "D_wl_group_blocks" = grp_blocks_2010$D_wl,
                              "D_bl_group_blocks" = grp_blocks_2010$D_bl,
                              "D_ba_group_blocks" = grp_blocks_2010$D_ba,
                              "D_la_group_blocks" = grp_blocks_2010$D_la,
                              
                              "D_wb_tract" = tract_2010$D_wb,
                              "D_wa_tract" = tract_2010$D_wa,
                              "D_wl_tract" = tract_2010$D_wl,
                              "D_bl_tract" = tract_2010$D_bl,
                              "D_ba_tract" = tract_2010$D_ba,
                              "D_la_tract" = tract_2010$D_la)
    
    ind_D_2000 <<- data.frame("D_wb_block" = block_2000$D_wb,
                              "D_wa_block" = block_2000$D_wa,
                              "D_wl_block" = block_2000$D_wl,
                              "D_bl_block" = block_2000$D_bl,
                              "D_ba_block" = block_2000$D_ba,
                              "D_la_block" = block_2000$D_la,
                              
                              "D_wb_group_blocks" = grp_blocks_2000$D_wb,
                              "D_wa_group_blocks" = grp_blocks_2000$D_wa,
                              "D_wl_group_blocks" = grp_blocks_2000$D_wl,
                              "D_bl_group_blocks" = grp_blocks_2000$D_bl,
                              "D_ba_group_blocks" = grp_blocks_2000$D_ba,
                              "D_la_group_blocks" = grp_blocks_2000$D_la,
                              
                              "D_wb_tract" = tract_2000$D_wb,
                              "D_wa_tract" = tract_2000$D_wa,
                              "D_wl_tract" = tract_2000$D_wl,
                              "D_bl_tract" = tract_2000$D_bl,
                              "D_ba_tract" = tract_2000$D_ba,
                              "D_la_tract" = tract_2000$D_la)
    
    ind_D_1990 <<- data.frame("D_wb_block" = block_1990$D_wb,
                              "D_wa_block" = block_1990$D_wa,
                              "D_wl_block" = block_1990$D_wl,
                              "D_bl_block" = block_1990$D_bl,
                              "D_ba_block" = block_1990$D_ba,
                              "D_la_block" = block_1990$D_la,
                              
                              "D_wb_group_blocks" = grp_blocks_1990$D_wb,
                              "D_wa_group_blocks" = grp_blocks_1990$D_wa,
                              "D_wl_group_blocks" = grp_blocks_1990$D_wl,
                              "D_bl_group_blocks" = grp_blocks_1990$D_bl,
                              "D_ba_group_blocks" = grp_blocks_1990$D_ba,
                              "D_la_group_blocks" = grp_blocks_1990$D_la,
                              
                              "D_wb_tract" = tract_1990$D_wb,
                              "D_wa_tract" = tract_1990$D_wa,
                              "D_wl_tract" = tract_1990$D_wl,
                              "D_bl_tract" = tract_1990$D_bl,
                              "D_ba_tract" = tract_1990$D_ba,
                              "D_la_tract" = tract_1990$D_la)

    
    # # # # entropia i entropia std
    
    ind_ent_2020 <<- data.frame("Entropy" = block_2020$Entropy,
                                "Entropy_std" = block_2020$Entropy_std)
    
    ind_ent_2010 <<- data.frame("Entropy" = block_2010$Entropy,
                                "Entropy_std" = block_2010$Entropy_std)
    
    ind_ent_2000 <<- data.frame("Entropy" = block_2000$Entropy,
                                "Entropy_std" = block_2000$Entropy_std)
    
    ind_ent_1990 <<- data.frame("Entropy" = block_1990$Entropy,
                                "Entropy_std" = block_1990$Entropy_std)
}
rozdzielenie()

#  #  #  #  #  #  #  #  #  #  #  #  #  #   POLACZENIE DANYCH Z DANYMI PRZESTRZENNYMI

shp_1990 <- read_sf("counties_shp/low_res/shp_1990.gpkg")
shp_2020 <- read_sf("counties_shp/low_res/shp_2020.gpkg")

# # usuniecie zbednych kolumn
 shp_1990 <- shp_1990[, -c(1:4)]
 shp_2020 <- shp_2020[, -c(3:4, 7:9)]

 
 # zmiana kolejnosci kolumn w pliku z 1990 r
 shp_1990 <- shp_1990[, c(1, 2, 4, 3, 5, 6, 7)]
 
 # ujednolicenie nazw kolumn
 colnames(shp_1990) <- c("STATEFP", "COUNTYFP", "GEOID", "NAME", "STUSPS", "STATE_NAME", "geom")

 #st_write(shp_2020, "counties_shp/low_res/shp_2020.gpkg")
 #st_write(shp_1990, "counties_shp/low_res/shp_1990.gpkg")
#shp_1990 <- read_sf("counties_shp/low_res/shp_1990.gpkg")
#shp_2020 <- read_sf("counties_shp/low_res/shp_2020.gpkg")

shp_2020 <- shp_2020 %>% arrange(COUNTYFP, STATEFP)
shp_1990 <- shp_1990 %>% arrange(COUNTYFP, STATEFP)


shp_join <- function(){
  ##dolaczenie geoid 1990-2000
  block_1990$GEOID <- shp_1990$GEOID
  block_2000$GEOID <- shp_1990$GEOID
  grp_blocks_1990$GEOID <- shp_1990$GEOID
  grp_blocks_2000$GEOID <- shp_1990$GEOID
  tract_1990$GEOID <- shp_1990$GEOID
  tract_2000$GEOID <- shp_1990$GEOID
  
  ind_D_1990$GEOID <- shp_1990$GEOID
  ind_D_2000$GEOID <- shp_1990$GEOID
  ind_ent_1990$GEOID <- shp_1990$GEOID
  ind_ent_2000$GEOID <- shp_1990$GEOID
  ind_H_1990$GEOID <- shp_1990$GEOID
  ind_H_2000$GEOID <- shp_1990$GEOID
  #2010-2020
  block_2010$GEOID <- shp_2020$GEOID
  block_2020$GEOID <- shp_2020$GEOID
  grp_blocks_2010$GEOID <- shp_2020$GEOID
  grp_blocks_2020$GEOID <- shp_2020$GEOID
  tract_2010$GEOID <- shp_2020$GEOID
  tract_2020$GEOID <- shp_2020$GEOID
  
  ind_D_2010$GEOID <- shp_2020$GEOID
  ind_D_2020$GEOID <- shp_2020$GEOID
  ind_ent_2010$GEOID <- shp_2020$GEOID
  ind_ent_2020$GEOID <- shp_2020$GEOID
  ind_H_2010$GEOID <- shp_2020$GEOID
  ind_H_2020$GEOID <- shp_2020$GEOID
  
  #utworzenie danych przestrzennych
  shp_block_1990 <<- left_join(shp_1990, block_1990 , by = c("GEOID" = "GEOID"))
  shp_block_2000 <<- left_join(shp_1990, block_2000 , by = c("GEOID" = "GEOID"))
  shp_block_2010 <<- left_join(shp_2020, block_2010 , by = c("GEOID" = "GEOID"))
  shp_block_2020 <<- left_join(shp_2020, block_2020 , by = c("GEOID" = "GEOID"))
  shp_grp_blocks_1990 <<- left_join(shp_1990, grp_blocks_1990, by = c("GEOID" = "GEOID"))
  shp_grp_blocks_2000 <<- left_join(shp_1990, grp_blocks_2000, by = c("GEOID" = "GEOID"))
  shp_grp_blocks_2010 <<- left_join(shp_2020, grp_blocks_2010, by = c("GEOID" = "GEOID"))
  shp_grp_blocks_2020 <<- left_join(shp_2020, grp_blocks_2020, by = c("GEOID" = "GEOID"))
  shp_tract_1990 <<- left_join(shp_1990, tract_1990 , by = c("GEOID" = "GEOID"))
  shp_tract_2000 <<- left_join(shp_1990, tract_2000 , by = c("GEOID" = "GEOID"))
  shp_tract_2010 <<- left_join(shp_2020, tract_2010 , by = c("GEOID" = "GEOID"))
  shp_tract_2020 <<- left_join(shp_2020, tract_2020 , by = c("GEOID" = "GEOID"))
  
  shp_ind_D_1990 <<- left_join(shp_1990, ind_D_1990, by = c("GEOID" = "GEOID"))
  shp_ind_D_2000 <<- left_join(shp_1990, ind_D_2000, by = c("GEOID" = "GEOID"))
  shp_ind_D_2010 <<- left_join(shp_2020, ind_D_2010, by = c("GEOID" = "GEOID"))
  shp_ind_D_2020 <<- left_join(shp_2020, ind_D_2020, by = c("GEOID" = "GEOID"))
  
  shp_ind_ent_1990 <<- left_join(shp_1990, ind_ent_1990, by = c("GEOID" = "GEOID"))
  shp_ind_ent_2000 <<- left_join(shp_1990, ind_ent_2000, by = c("GEOID" = "GEOID"))
  shp_ind_ent_2010 <<- left_join(shp_2020, ind_ent_2010, by = c("GEOID" = "GEOID"))
  shp_ind_ent_2020 <<- left_join(shp_2020, ind_ent_2020, by = c("GEOID" = "GEOID"))
  
  shp_ind_H_1990 <<- left_join(shp_1990, ind_H_1990, by = c("GEOID" = "GEOID"))
  shp_ind_H_2000 <<- left_join(shp_1990, ind_H_2000, by = c("GEOID" = "GEOID"))
  shp_ind_H_2010 <<- left_join(shp_2020, ind_H_2010, by = c("GEOID" = "GEOID"))
  shp_ind_H_2020 <<- left_join(shp_2020, ind_H_2020, by = c("GEOID" = "GEOID"))
}
shp_join()

 zapis_wynikow <- function(){
  st_write(shp_block_1990, "counties_shp/low_res/census_shp/shp_block_1990.gpkg")
  st_write(shp_block_2000, "counties_shp/low_res/census_shp/shp_block_2000.gpkg")
  st_write(shp_block_2010, "counties_shp/low_res/census_shp/shp_block_2010.gpkg")
  st_write(shp_block_2020, "counties_shp/low_res/census_shp/shp_block_2020.gpkg")
  
  st_write(shp_grp_blocks_1990, "counties_shp/low_res/census_shp/shp_grp_blocks_1990.gpkg")
  st_write(shp_grp_blocks_2000, "counties_shp/low_res/census_shp/shp_grp_blocks_2000.gpkg")
  st_write(shp_grp_blocks_2010, "counties_shp/low_res/census_shp/shp_grp_blocks_2010.gpkg")
  st_write(shp_grp_blocks_2020, "counties_shp/low_res/census_shp/shp_grp_blocks_2020.gpkg")
  
  st_write(shp_tract_1990, "counties_shp/low_res/census_shp/shp_tract_1990.gpkg")
  st_write(shp_tract_2000, "counties_shp/low_res/census_shp/shp_tract_2000.gpkg")
  st_write(shp_tract_2010, "counties_shp/low_res/census_shp/shp_tract_2010.gpkg")
  st_write(shp_tract_2020, "counties_shp/low_res/census_shp/shp_tract_2020.gpkg")
  
  st_write(shp_ind_D_1990, "counties_shp/low_res/indexes_shp/shp_ind_D_1990.gpkg")
  st_write(shp_ind_D_2000, "counties_shp/low_res/indexes_shp/shp_ind_D_2000.gpkg")
  st_write(shp_ind_D_2010, "counties_shp/low_res/indexes_shp/shp_ind_D_2010.gpkg")
  st_write(shp_ind_D_2020, "counties_shp/low_res/indexes_shp/shp_ind_D_2020.gpkg")
  
  st_write(shp_ind_ent_1990, "counties_shp/low_res/indexes_shp/shp_ind_ent_1990.gpkg")
  st_write(shp_ind_ent_2000, "counties_shp/low_res/indexes_shp/shp_ind_ent_2000.gpkg")
  st_write(shp_ind_ent_2010, "counties_shp/low_res/indexes_shp/shp_ind_ent_2010.gpkg")
  st_write(shp_ind_ent_2020, "counties_shp/low_res/indexes_shp/shp_ind_ent_2020.gpkg")
  
  st_write(shp_ind_H_1990, "counties_shp/low_res/indexes_shp/shp_ind_H_1990.gpkg")
  st_write(shp_ind_H_2000, "counties_shp/low_res/indexes_shp/shp_ind_H_2000.gpkg")
  st_write(shp_ind_H_2010, "counties_shp/low_res/indexes_shp/shp_ind_H_2010.gpkg")
  st_write(shp_ind_H_2020, "counties_shp/low_res/indexes_shp/shp_ind_H_2020.gpkg")
  
}
# zapis_wynikow()
shp_tract_1990 <- st_read("counties_shp/low_res/census_shp/shp_tract_1990.gpkg")
shp_tract_2020 <- st_read("counties_shp/low_res/census_shp/shp_tract_2020.gpkg")
shp_ind_H_1990 <- st_read("counties_shp/low_res/indexes_shp/shp_ind_H_1990.gpkg")
shp_ind_H_2020 <- st_read("counties_shp/low_res/indexes_shp/shp_ind_H_2020.gpkg")


#  #  #  #  #  #  #  #  #  #  #  #  #  #  WIZUALIZACJA
tmap_mode("view")
tm_shape(shp_ind_H_1990) + tm_fill(col = "H_block") + tm_borders() +
  tm_shape(shp_ind_H_2020) + tm_fill(col = "H_block") + tm_borders()


tm_shape(shp_tract_2020) + tm_fill(col = "Entropy_std", 
                         id = "NAMELSAD",
                         popup.vars = c("Entropy: " = "Entropy", "Entropy std: " = "Entropy_std", 
                                        "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
                                        "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
                                        "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")) + tm_borders()





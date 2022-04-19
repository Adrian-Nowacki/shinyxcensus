
# PRACA INŻYNIERSKA - ADRIAN NOWACKI


#  #  #  #  #  #  #  #  #  #  #  #  #  #  WCZYTANIE DANYCH I PAKIETÓW

library(dplyr)
library(sf)
library(tmap)

    # # # ROK 1990
    block_1990 = read.csv("../1990/nhgis0105_ds120_1990_block.csv")
    grp_blocks_1990 = read.csv("../1990/nhgis0105_ds120_1990_blck_grp.csv")
    tract_1990 = read.csv("../1990/nhgis0105_ds120_1990_tract.csv") 
    
    # # # ROK 2000 
    block_2000 = read.csv("../2000/nhgis0106_ds147_2000_block.csv")
    grp_blocks_2000 = read.csv("../2000/nhgis0106_ds147_2000_blck_grp.csv")
    tract_2000 = read.csv("../2000/nhgis0107_ds146_2000_tract.csv")
    
    # # # ROK 2010
    block_2010 = read.csv("../2010/nhgis0105_ds172_2010_block.csv")
    grp_blocks_2010 = read.csv("../2010/nhgis0105_ds172_2010_blck_grp.csv")
    tract_2010 = read.csv("../2010/nhgis0105_ds172_2010_tract.csv")
    
    # # # ROK 2020 
    
    block_2020 = read.csv("../2020/nhgis0105_ds248_2020_block.csv")
    grp_blocks_2020 = read.csv("../2020/nhgis0105_ds248_2020_blck_grp.csv")
    tract_2020 = read.csv("../2020/nhgis0105_ds248_2020_tract.csv")
    
    
    
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
  colnames(Ent_indexes) <- c("Entropia", "Entropia_std")                  
  Ent_indexes <- round(Ent_indexes, 4)
  county_num <- group_keys(grpd)                                        # wyszczegolnienie kodu zlaczenia danych
  Ent_indexes <- cbind(county_num, Ent_indexes)                        # ramka danych z kodem stanu, hrabstwa oraz dwoma wskaznikami
}

index_H <- function(x){
  grpd <- x %>% group_by(COUNTYA, STATEA) %>% arrange(COUNTYA)          # pogrupowanie danych wedlug kodu hrabstwa i stanu
  splt <- group_split(grpd)
  races <- lapply(splt, function(splt) splt[!(names(splt) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))])
  
  h_indexes <- lapply(races, h_index)
  
  h_indexes <- do.call(rbind.data.frame, h_indexes)
  colnames(h_indexes) <- "H"
  h_indexes <- round(h_indexes, 4)                                     # ramka danych ze wskaznikiem H dla kazdego hrabstwa
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
    block_2020 <<- as.data.frame(list[4])
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


#  #  #  #  #  #  #  #  #  #  #  #  #  #   POLACZENIE DANYCH Z DANYMI PRZESTRZENNYMI

shp <- read_sf("../dane_shp/przyciete.gpkg")

shp$COUNTYFP <- as.numeric(substring(shp$COUNTYFP, 2))                   # usuniecie 0 z poczatku kodu hrabstwa w celu polaczenia danych
shp <- shp[, -(c(1, 4:5, 8:20))]                                         # usuniecie zbednych kolumn

shp_join <- function(){
  shp_block_1990 <<- left_join(shp, block_1990 , by = c("COUNTYFP" = "COUNTYA"))
  shp_block_2000 <<- left_join(shp, block_2000 , by = c("COUNTYFP" = "COUNTYA"))
  shp_block_2010 <<- left_join(shp, block_2010 , by = c("COUNTYFP" = "COUNTYA"))
  shp_block_2020 <<- left_join(shp, block_2020 , by = c("COUNTYFP" = "COUNTYA"))
  shp_grp_blocks_1990 <<- left_join(shp, grp_blocks_1990, by = c("COUNTYFP" = "COUNTYA"))
  shp_grp_blocks_2000 <<- left_join(shp, grp_blocks_2000, by = c("COUNTYFP" = "COUNTYA"))
  shp_grp_blocks_2010 <<- left_join(shp, grp_blocks_2010, by = c("COUNTYFP" = "COUNTYA"))
  shp_grp_blocks_2020 <<- left_join(shp, grp_blocks_2020, by = c("COUNTYFP" = "COUNTYA"))
  shp_tract_1990 <<- left_join(shp, tract_1990 , by = c("COUNTYFP" = "COUNTYA"))
  shp_tract_2000 <<- left_join(shp, tract_2000 , by = c("COUNTYFP" = "COUNTYA"))
  shp_tract_2010 <<- left_join(shp, tract_2010 , by = c("COUNTYFP" = "COUNTYA"))
  shp_tract_2020 <<- left_join(shp, tract_2020 , by = c("COUNTYFP" = "COUNTYA"))
  
  
}
shp_join()


tmap_mode("view")
tm_shape(shp_block_1990) + tm_fill(col = "Entropia", 
                         id = "NAMELSAD",
                         popup.vars = c("Entropia: " = "Entropia", "Entropia std: " = "Entropia_std", 
                                        "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
                                        "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
                                        "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")) + tm_borders()

##write_sf(shp_block_1990, "../dane_shp/shp_block_1990.gpkg")

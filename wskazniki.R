#  #  #  #  #  #  #  FUNCTIONS CALCULATING INDICATORS 

    # # # entropy
    entropy_fnc = function(proportions, base = exp(1)) {
      entr = -sum(ifelse(proportions > 0, proportions * log(proportions, base = base), 0))
      return(entr)
    } 
    
    
    # # # entropy std
    entropy_std_fnc = function(proportions, base = exp(1)) {
      entr = -sum(ifelse(proportions > 0, proportions * log(proportions, base = base), 0)) #obliczenie entropii
      entr_std = entr/log(length(proportions), base = base) #standaryzacja
      return(entr_std)
    }
    
    
    # # # wskaźnik niepodobieństwa D
    # a - liczba osob grupy 1 w jednostce spisowej, b - liczba osob gruy 2 w jednostce spisowej
    d_ind = function(a, b) {
      d = 0.5*sum(abs(a/sum(a, na.rm=TRUE) - b/sum(b, na.rm=TRUE)))
      return(d)
    }
    
    
    # # # wskaźnik teorii informacji H.
    hindex <- function(races) {
      races_all = apply(races, 2, sum, na.rm=TRUE) # liczba osob w calym obszarze w podziale na grupy rasowo-etniczne
      
      pop = sum(races_all, na.rm=TRUE)  #liczba osob w calym obszarze
      
      pop_i = apply(races, 1, sum, na.rm=TRUE)  #liczba osob w kazdej jednostce spisowej
      
      proportions = races/pop_i  #odsetek osob w danej grupy rasowo-etnicznej w kazdej jednostce spisowej
      
      proportions_all = races_all/sum(races_all, na.rm = TRUE)  #odsetek osob w danej grupy rasowo-etnicznej w calym obszarze
      
      ent_i = apply(proportions, 1, entropy_fnc)  #entropia dla kazdej jednostki spisowej
      
      ent = entropy_fnc(proportions_all) #entropia dla calego obszaru
      
      hind = sum(pop_i*(ent-ent_i)/(ent*pop), na.rm=TRUE)  #obliczenie H
      return(hind)
    }

    
    
    
list_race <- c("white", "black", "american", "asian", "other", "latin")
    
index_entropia <- function(x){
      grpd <- x %>% group_by(COUNTYA, STATEA) %>% arrange(COUNTYA)
      splt <- group_split(grpd )
      races <- lapply(splt, function(splt) splt[!(names(splt) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))])
      races_all <- lapply(races, colSums, na.rm = TRUE)
      pop = lapply(races_all, sum)
      perc <- Map("/", races_all, pop)
      ent  <- lapply(perc, entropy_fnc)
      
      #entropia standaryzowana
      Ent_std_index <- lapply(perc, entropy_std_fnc)
      ent_std_table <- do.call(rbind.data.frame, Ent_std_index)
      
      #tabela
      ent_table <- do.call(rbind.data.frame, ent)
      Ent_index <<- cbind(ent_table, ent_std_table)
      colnames(Ent_index) <- c("Entropia", "Entropia_std")
      Ent_index <- round(Ent_index, 4)
      county_num <- group_keys(grpd)
      Ent_index <<- cbind(county_num, Ent_index)
}

index_H <- function(x){
    grpd <- x %>% group_by(COUNTYA, STATEA) %>% arrange(COUNTYA)
    splt <- group_split(grpd)
    races <- lapply(splt, function(splt) splt[!(names(splt) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))])
    
    h_indexes <- lapply(races, hindex)
    
    h_indexes <- do.call(rbind.data.frame, h_indexes)
    colnames(h_indexes) <- "H"
    h_indexes <<- round(h_indexes, 4)
    #county_num <- group_keys(grpd)
    #H_ind <<- cbind(county_num, h_indexes)
}

index_D <- function(x){
  grpd <- x %>% group_by(COUNTYA, STATEA) %>% arrange(COUNTYA)
  splt <- group_split(grpd)
  races <- lapply(splt, function(splt) splt[!(names(splt) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))])

  white <- sapply(races, function(x) x%>% select(white))
  black <- sapply(races, function(x) x%>% select(black))
  asian <- sapply(races, function(x) x%>% select(asian))
  other <- sapply(races, function(x) x%>% select(other))
  latin <- sapply(races, function(x) x%>% select(latin))
  
  D_wb <- as.data.frame(mapply(d_ind, white, black))
  D_wa <- as.data.frame(mapply(d_ind, white, asian))
  D_wl <- as.data.frame(mapply(d_ind, white, latin))
  D_bl <- as.data.frame(mapply(d_ind, black, latin))
  D_ba <- as.data.frame(mapply(d_ind, black, asian))
  D_la <- as.data.frame(mapply(d_ind, latin, asian))
  
  D_indexes <- cbind(D_wb, D_wa, D_wl, D_bl, D_ba, D_la)
  colnames(D_indexes) <- c("D_wb", "D_wa", "D_wl", "D_bl", "D_ba", "D_la")
  D_indexes <<- round(D_indexes, 4)
  #county_num <- group_keys(grpd)
  #D_ind <<- cbind(county_num, D_indexes)
}


 all_files <- list(block_1990, block_2000, block_2010, block_2020, grp_blocks_1990, grp_blocks_2000, 
                grp_blocks_2010, grp_blocks_2000, tract_1990, tract_2000, tract_2010, tract_2000)

       indexes <- function(){
         for (i in length(all_files)){
           ent_list <- lapply(all_files[1:i], index_entropia)
           H_list <- lapply(all_files[1:i], index_H)
           D_list <- lapply(all_files[1:i], index_D)
           list<- mapply(function(a, b, c) {
             out <- cbind(a, b, c)
             out
           }, ent_list, H_list, D_list, SIMPLIFY = FALSE)
           
           block_1990 <<- as.data.frame(list[1])
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


#  #  #  #  #  #  POLACZENIE DANYCH Z SHP
    
    shp <- read_sf("../dane_shp/przyciete.gpkg")
    
    shp$COUNTYFP <- as.numeric(substring(shp$COUNTYFP, 2))
    
    shp1 <- left_join(shp, block_1990 , by = c("COUNTYFP" = "COUNTYA"))
    shp2 <- left_join(shp, block_2000 , by = c("COUNTYFP" = "COUNTYA"))
    tmap_mode("view")
    tm_shape(shp1) + tm_fill(col = "Entropia", 
                             interactive = TRUE,
                             id = "NAME",
                             popup.vars = c("Entropia: " = "Entropia", "Entropia std: " = "Entropia_std", 
                                            "H: " = "H", "D (white-black)" = "D_wb"),
                             popup.format=list(growth=list(digits=3))) + tm_borders()
      
      
      # tm_shape(shp2) + tm_polygons(col = "Entropia") +
      # tm_shape(shp3) + tm_polygons(col = "Entropia") +
      # tm_shape(shp4) + tm_polygons(col = "Entropia")
      
    
    

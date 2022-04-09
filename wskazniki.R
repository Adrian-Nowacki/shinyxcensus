#  #  #  #  #  #  #  FUNCTIONS CALCULATING INDICATORS 

    # # # entropia
    entropy_fnc = function(proportions, base = exp(1)) {
      entr = -sum(ifelse(proportions > 0, proportions * log(proportions, base = base), 0))
      return(entr)
    } 
    
    
    # # # entropia standaryzowana
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
    
    # races_all  <- apply(block_1990[,list_race], 2, sum, na.rm = TRUE)
    # pop = sum(races_all)
    # perc <- races_all/pop
    # 
    # ent  <- entropy_fnc(perc)
    # ent_std <- entropy_std_fnc(perc)
    # 
    # # obliczenei H
    # races = block_1990[, list_race]
    # h = hindex(races)
    # 
    # # wskaznik D
    # d_wb <- d_ind(hamilton1990$white, hamilton1990$black)
    # d_wa <- d_ind(hamilton1990$white, hamilton1990$asian)
    # d_wl <- d_ind(hamilton1990$white, hamilton1990$latin)
    # d_bl <- d_ind(hamilton1990$black, hamilton1990$latin)
    # d_al <- d_ind(hamilton1990$asian, hamilton1990$latin)
    # d_ab <- d_ind(hamilton1990$asian, hamilton1990$black)
    # 
    # 
    
    
    # obliczenei H
   # races = count2[, list_race]
    #h = hindex(races)
    #h
    
      #count2 <- block_1990[block_1990$COUNTYA == "61",]
x <- block_1990 %>% group_by(COUNTYA) %>% arrange(COUNTYA)
    
a <- group_split(x)

races <- lapply(a, function(a) a[!(names(a) %in% c("GISJOIN", "YEAR", "COUNTYA", "STATEA"))])

h_indexes <- lapply(races, hindex)

class(h_indexes)
h_indexes <- do.call(rbind.data.frame, h_indexes)
colnames(h_indexes) <- "H"
View(h_indexes)
          # for (i in races){
          #   print(hindex(i))
          #  
          #   
          #   #b <<- lapply(races[1:i], hindex(races[1:i]))
          # }


# 
# i <- as.data.frame(races[1])
# h <- hindex(i)
# apply(races, hindex(races[1]))
# pomiary = lapply(races, FUN = hindex)
# 
# as.data.frame(pomiary)

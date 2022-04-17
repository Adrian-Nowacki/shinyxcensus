library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

#  #  #  #  FILES

    # # # YEAR 1990
    block_1990 = read.csv("../1990/nhgis0105_ds120_1990_block.csv")
    grp_blocks_1990 = read.csv("../1990/nhgis0105_ds120_1990_blck_grp.csv")
    tract_1990 = read.csv("../1990/nhgis0105_ds120_1990_tract.csv") 
    
    # # # YEAR 2000 
    block_2000 = read.csv("../2000/nhgis0106_ds147_2000_block.csv")
    grp_blocks_2000 = read.csv("../2000/nhgis0106_ds147_2000_blck_grp.csv")
    tract_2000 = read.csv("../2000/nhgis0107_ds146_2000_tract.csv")
    
    # # # YEAR 2010
    block_2010 = read.csv("../2010/nhgis0105_ds172_2010_block.csv")
    grp_blocks_2010 = read.csv("../2010/nhgis0105_ds172_2010_blck_grp.csv")
    tract_2010 = read.csv("../2010/nhgis0105_ds172_2010_tract.csv")
    
    # # # YEAR 2020 
    
    block_2020 = read.csv("../2020/nhgis0105_ds248_2020_block.csv")
    grp_blocks_2020 = read.csv("../2020/nhgis0105_ds248_2020_blck_grp.csv")
    tract_2020 = read.csv("../2020/nhgis0105_ds248_2020_tract.csv")
    
    
    
   # change of column names from "FMS" to "FYF"
# names(tract_2000)[31:39]<- paste0("FYF00", 1:9)
# names(tract_2000)[40:44]<- paste0("FYF0", 10:14)


    all_files = list(block_1990, grp_blocks_1990, tract_1990, block_2000, grp_blocks_2000, 
                     tract_2000, block_2010, grp_blocks_2010, tract_2010, block_2020, 
                     grp_blocks_2020, tract_2020)
    
# shp data 
   #  county <- read_sf("dane_shp/US_county_2020.shp")


#  #  #  #  #  #  #  #  #  #  #  #  #  #  RECLASSIFICATION


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


#  #  #  #  #  #  #  #  #  #  #  #  #  #  




#  #  #  #  #  #  #  #  #  #  #  #  #  #  CALCULATION OF INDICATORS
#dane = dane1990[1:250, 27:36]

funct <- function(){
  dane =  mutate(dane, suma = dane$white + dane$black + dane$american + dane$asian + dane$other + dane$latin)
  
  white = dane$white
  black = dane$black
  american = dane$american
  asian = dane$asian
  other = dane$other
  latin = dane$latin
  suma = dane$suma
  
  
  w_suma = dane$white/dane$suma
  b_suma = dane$black/dane$suma
  am_suma = dane$american/dane$suma
  a_suma = dane$asian/dane$suma
  o_suma = dane$other/dane$suma
  l_suma = dane$latin/dane$suma
  
  w_suma = replace(w_suma, w_suma == 0, 1)
  b_suma = replace(b_suma, b_suma == 0, 1)
  am_suma = replace(am_suma, am_suma == 0, 1)
  a_suma = replace(a_suma, a_suma == 0, 1)
  o_suma = replace(o_suma, o_suma == 0, 1)
  l_suma = replace(l_suma, l_suma == 0, 1)
  
  # obliczenie entropii
  dane <- mutate(dane, "entropia" = -(white/suma * log(w_suma) + black/suma * log(b_suma) + 
                                   american/suma * log(am_suma) + asian/suma * log(a_suma) +
                                   other/suma * log(o_suma) + latin/suma * log(l_suma)))
  
  # obliczenie wskaznika niepodobienstwa D
  dane <<- mutate(dane, "D (w-b)" = round(sum(1/2 * abs(white/sum(white) - black/sum(black))), 3),
                  "D [w-a]" = round(sum(1/2 * abs(white/sum(white) - asian/sum(asian))), 3),
                  "D (w-l)" = round(sum(1/2 * abs(white/sum(white) - latin/sum(latin))), 3),
                  "D (b-l)" = round(sum(1/2 * abs(black/sum(black) - latin/sum(latin))), 3),
                  "D (b-a)" = round(sum(1/2 * abs(black/sum(black) - asian/sum(asian))), 3),
                  "D (l-a)" = round(sum(1/2 * abs(latin/sum(latin) - asian/sum(asian))), 3))
  
  # obliczanie wskaznika teorii informacji H
  
    # entropia obszaru
     entropia_obszaru = -(sum(white)/sum(suma) * log(sum(white)/sum(suma)) + 
                          sum(black)/sum(suma) * log(sum(black)/sum(suma)) + 
                          sum(american)/sum(suma) * log(sum(american)/sum(suma)) + 
                          sum(asian)/sum(suma) * log(sum(asian)/sum(suma)) +
                          sum(other)/sum(suma) * log(sum(other)/sum(suma)) + 
                          sum(latin)/sum(suma) * log(sum(latin)/sum(suma)))
      
     
     # wskaznik H
     H = sum(dane$suma/sum(dane$suma) * ((entropia_obszaru - dane$entropia)/entropia_obszaru), na.rm = TRUE)
  
    
    # ramka danych zawierajaca skumulowany wskaznik niepodobienstwa D
  ramka <<- data.frame("Rok" = "1990", 
                       "D_wb" = round(sum(1/2 * abs(white/sum(white) - black/sum(black))), 3),
                       "D_wa" = round(sum(1/2 * abs(white/sum(white) - asian/sum(asian))), 3),
                       "D_wl" = round(sum(1/2 * abs(white/sum(white) - latin/sum(latin))), 3),
                       "D_bl" = round(sum(1/2 * abs(black/sum(black) - latin/sum(latin))), 3),
                       "D_ba" = round(sum(1/2 * abs(black/sum(black) - asian/sum(asian))), 3),
                       "D_la" = round(sum(1/2 * abs(latin/sum(latin) - asian/sum(asian))), 3),
                       "entropia_obsz" = entropia_obszaru,
                       "H" = round(H, 3))
  
  }

funct()

















df <- data.frame(ID = c("C1", "C2", "C3", "C4", "C5", "C6", "C7"),
                 white = as.numeric(c("70", "0", "10", "0", "40", "0", "20")),
                 black = as.numeric(c("0", "10", "20", "50", "0", "0", "20")),
                 asian = as.numeric(c("10", "40", "30", "0", "40", "0", "20")),
                 latin = as.numeric(c("0", "30", "20", "30", "0", "80", "20")))

df = df %>% mutate(suma = df$white + df$black + df$asian +df$latin)

##df <- mutate(df, entropia = -(white/suma * log(white/suma) + black/suma * log(black/suma) + 
##                       asian/suma * log(asian/suma) + latin/suma * log(latin/suma)))

funct <- function(){
  w_suma = df$white/df$suma
  b_suma = df$black/df$suma
  a_suma = df$asian/df$suma
  l_suma = df$latin/df$suma
  
  w_suma = replace(w_suma, w_suma == 0, 1)
  b_suma = replace(b_suma, b_suma == 0, 1)
  a_suma = replace(a_suma, a_suma == 0, 1)
  l_suma = replace(l_suma, l_suma == 0, 1)
  df <<- mutate(df, entropia = -(white/suma * log(w_suma) + black/suma * log(b_suma) + 
                                   asian/suma * log(a_suma) + latin/suma * log(l_suma)))
}

funct()


#df <- mutate(df, liczba = rowSums(df[2:5] !=0),
#             entropia = -(white/suma * log(1/liczba) + black/suma * log(1/liczba) + 
#                            asian/suma * log(1/liczba) + latin/suma * log(1/liczba)))

## dobrze
dane = read.csv("cw2_dane/przykladowe_dane.csv")
df <- apply(dane[, -1], 2, sum)
pop <- sum(df)
p <- df/pop
e <- sum(p*log(p))
sum(dane$BIALI)

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 1, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

stopifnot( apply(x, 2, is.vector))

df <- dane[, "BIALI"]
summary(dane)

library(data.table)
library(magrittr)

geo_file <- fread(paste0("nygeo2010.ur1"),sep = "\n", header = FALSE)
dim(geo_file)
head(geo_file, 1)
View(geo_file)
geo <- geo_file[, .(LOGRECNO = as.numeric(substr(V1, 19, 25)),
                    SUMLEV = substr(V1, 9, 11),
                    PLACE = substr(V1, 46, 50),
                    INTPTLAT = as.numeric(substr(V1, 337, 347)),
                    INTPTLON = as.numeric(substr(V1, 348, 359)))]
geo
View(geo)

syr_geo <- geo[PLACE == "73000"]
syr_geo

#summary level 100 corresponds to urban/rural level and sumlevel=100 has all the required geographic information.
syr_block <- syr_geo[SUMLEV == "100"]
syr_block

map <- get_map("syracuse, New York", zoom = 13)
ggmap(map) +
  geom_point(data = syr_block, aes(INTPTLON, INTPTLAT), color = "red", alpha = 0.3)


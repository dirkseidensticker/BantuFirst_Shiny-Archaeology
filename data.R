###########
# Dataset # 
###########

library(sf)

# SPACIALIST DB
source("../conf.R")

epsg <- 4326
crs <- paste("+init=epsg:",epsg,"", sep="")

# Comparative Sites:
s <- dbGetQuery(con, 
"SELECT 
  e.id,
  e.name, 
  ST_X(ST_AsEWKT(g.geom)),
  ST_Y(ST_AsEWKT(g.geom))
FROM
  entities e,
  geodata g
WHERE 
  g.id = e.geodata_id AND
  e.entity_type_id = 3;")

write.csv(s, "s.csv", row.names=FALSE)

# BantuFirst Sites:
bf.loc <- dbGetQuery(con, "
SELECT 
  e.id,
  e.name,
  ST_AsEWKT(g.geom) AS geom
FROM
  entities e,
  geodata g
WHERE 
  g.id = e.geodata_id AND
  e.entity_type_id = 4")

bf.loc <- st_as_sf(bf.loc, wkt = "geom")

bf.surv <- dbGetQuery(con, "
SELECT 
  e.id,
  e.name,
  ST_AsEWKT(g.geom) AS geom
FROM
  entities e,
  geodata g
WHERE 
  g.id = e.geodata_id AND
  e.entity_type_id = 6")

bf.surv <- st_centroid(st_as_sf(bf.surv, wkt = "geom"))

bf <- rbind(bf.surv, bf.loc)

st_write(bf, "bf.csv", layer_options = "GEOMETRY=AS_WKT", delete_dsn = TRUE)

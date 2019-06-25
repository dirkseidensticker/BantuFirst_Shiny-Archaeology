###########
# Dataset # 
###########

library(jsonlite)
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
# =================
# BC:
# ---

bf.loc <- dbGetQuery(con, "
SELECT 
  e.id,
  e.name,
  ST_AsEWKT(g.geom) AS geom,
  av.json_val
FROM
  entities e LEFT JOIN attribute_values av ON e.id = av.entity_id,
  geodata g
WHERE 
  g.id = e.geodata_id AND
  e.entity_type_id = 4")

# chop json_val appart
json.val = NULL
for(i in 1:nrow(bf.loc)){
  if(!is.na(bf.loc$json_val)){
  val <- jsonlite::fromJSON(bf.loc$json_val[i])
  val$entityid <- bf.loc$id[i]
  json.val <- rbind(json.val, val)}
}

# get thesaurus labels:
th.labs <- dbGetQuery(con, 
"SELECT 
  thc.id,
  thc.concept_url,
  thcl.label
FROM
  th_concept thc,
  th_concept_label thcl
WHERE 
  thc.id = thcl.concept_id AND
  thcl.language_id = 1
")

# Merge json vals with labels and coords:
json.val.labs <- merge(x = json.val, y = th.labs, by = "concept_url")

bf.loc.chrono <- merge(x = json.val.labs, y = bf.loc, by.x = "entityid", by.y = "id")

StilGrChrono <- read.csv("https://raw.githubusercontent.com/dirkseidensticker/nwCongo/master/bib/StilGrChrono.csv", encoding = "UTF-8")

# clean up chronosites
stylechrono <- StilGrChrono[, c("Stilgruppe", "FROM", "TO")]

bf.loc.chrono.ft <- merge(x = bf.loc.chrono, y = stylechrono, by.x = "label", by.y = "Stilgruppe")

bf.loc.chrono.ft <- bf.loc.chrono.ft[,c("name", "geom", "FROM", "TO")]
bf.loc.chrono.ft <- st_as_sf(bf.loc.chrono.ft, wkt = "geom")

st_write(bf.loc.chrono.ft, "bf_chrono.csv", layer_options = "GEOMETRY=AS_WKT", delete_dsn = TRUE)

# without chronological details:

bf.loc <- st_as_sf(bf.loc[, !names(bf.loc) == "json_val"], wkt = "geom")

# DS:
# ---
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

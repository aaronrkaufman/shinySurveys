setwd("D:/Dropbox/Shiny Surveys/replication code/rollcall_lookup")
library(ggmap)
require(rgdal)
require(sp)
require(maps)
require(tigris)

districts <- congressional_districts(cb = TRUE, resolution = '20m')
d2 = as.data.frame(districts)
d2$STATEFP = gsub("(?<![0-9])0+", "", d2$STATEFP, perl = TRUE)
d2$CD115FP = gsub("(?<![0-9])0+", "", d2$CD115FP, perl = TRUE)
d2$dist_id = paste(d2$STATEFP, d2$CD115FP, sep="_")

dat = read.csv("legislators-current.csv")
dat = dat[dat$type == "rep",]
data("state.fips")
dat = merge(dat, state.fips, by.x="state", by.y="abb")
dat$dist_id = paste(dat$fips, dat$district, sep="_")

d3 = merge(d2, dat, by="dist_id")
d3 = d3[,c("dist_id", "first_name", "last_name")]
d3$rep = paste(d3$first_name, d3$last_name, sep=" ")

save(d3, d2, file="rep_data.RData")

star = Sys.time()
g = geocode("71 Dana St, Cambridge, MA")
coordinates(g) <- ~ lon + lat
proj4string(sp)<-proj4string(districts)
temp = function(x){
  v1 = SpatialPolygons(list(districts@polygons[[x]]))
  proj4string(v1) = proj4string(districts)
  !is.na(sp::over(sp, v1))
}
out = which(sapply(1:437, temp))
did = d2$dist_id[out]
rep = d3$rep[d3$dist_id==did][1]
Sys.time() - star # less than 2 seconds
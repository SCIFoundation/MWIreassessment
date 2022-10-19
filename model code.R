

library(plyr)
library(tidyr)
library(sf)
library(tmap)
library(spData)
library(viridis)
library(lubridate)
library(tidyverse)
library (maptools)
library (rgdal) 
library(sp)
library(ggplot2)
library(dplyr)
library(rgeos)
library(rgdal)
library(Matrix)
library(foreach)
library(INLA)
library(lwgeom)
library(raster)
library(PrevMap)
library(geoR)
library(reshape2)
library(leaflet)
library(htmlwidgets)


m <- getData(name = "GADM", country = "Malawi", level = 0)
plot(m)
m <- st_as_sf(m)


m <- m %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1)
m <- m %>% st_transform(25830)
ggplot(m) + geom_sf() + theme_bw()+ coord_sf(datum = st_crs(m))



map <-  readOGR("C:/Users/XIAORUOTONG/Documents/MWIreassessment-main/2016-11-30_Traditional Authority boundaries/MWI_adm2.shp")
plot(map)



rea_Map_0 <- read.csv("C:/Users/XIAORUOTONG/Downloads/MWIreassessment-main/MWI_Reassessment_Mapping_data.csv")



rea_Map_0$age_0 <- rea_Map_0$Age%/%100
rea_Map_0$age_1 <- rea_Map_0$Age%%100



r <- getData(name = "alt", country = "MW", mask = TRUE)
rea_Map_0$alt <- raster::extract(r, rea_Map_0 [, c("Long", "Lat")])



p <- st_as_sf(data.frame(long = rea_Map_0$Long, lat = rea_Map_0$Lat),
              coords = c("long", "lat"),agr = "constant")
st_crs(p) <- st_crs(4326)
p <- p %>% st_transform(25830)
rea_Map_0[, c("x", "y")] <- st_coordinates(p)


head(rea_Map_0)


ggplot(m) + geom_sf() + coord_sf(datum = st_crs(m)) +
  geom_point(data = rea_Map_0, aes(x = x, y = y)) + theme_bw()



bb <- st_bbox(m)
x <- seq(bb$xmin - 1, bb$xmax + 1, length.out = 15)
y <- seq(bb$ymin - 1, bb$ymax + 1, length.out = 40)
dp <- as.matrix(expand.grid(x, y))
plot(dp, asp = 1)



p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]),
  coords = c("x", "y")
)
st_crs(p) <- st_crs(25830)
ind <- st_intersects(m, p)
dp <- dp[ind[[1]], ]
plot(dp, asp = 1)





coo <- cbind(rea_Map_0$Long, rea_Map_0$Lat)
mesh <- inla.mesh.2d(
  loc = coo, 
  cutoff = 0.03, max.edge = c(0.5, 4)
)
plot(mesh)
points(coo, col = "red")



bnd <- inla.nonconvex.hull(coo)
meshb <- inla.mesh.2d(
  boundary = bnd,
  cutoff = 0.03, max.edge = c(0.5, 4)
)
plot(meshb)
points(coo, col = "red")


spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)



dp <- rasterToPoints(r)
dim(dp)
ra <- aggregate(r, fact = 5, fun = mean)

dp <- rasterToPoints(ra)
dim(dp)
coop <- dp[, c("x", "y")]
Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
dim(Ap)



A <- inla.spde.make.A(mesh = mesh, loc = coo)
dim(A)
nrow(coo)
Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
dim(Ap) 


indexs <- inla.spde.make.index("s", spde$n.spde)





stk.e <- inla.stack(
  tag = "est",
  data = list(y = rea_Map_0$Num_pos_anySTH, numtrials =rea_Map_0$Num_sampled_anySTH),
  A = list(1,A),
  effects = list(data.frame(b0 = 1, altitude = rea_Map_0$alt, age_0 = rea_Map_0$age_0,age_1 = rea_Map_0$age_1,year =rea_Map_0$Year ), s = indexs )
  #effects = list(data.frame(b0 = rep(1, nrow(his_Map_0))),  indexs )
  #effects = list(data.frame(b0 = 1, altitude = his_Map_0$alt), s = indexs)
  #,list(age_0 = Rea_Map_0$age_0,age_1 = Rea_Map_0$age_1,altitude = Rea_Map_0$alt,year = Rea_Map_0$Year)
  
)

# stack for prediction stk.p
stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA, numtrials =NA),
    A = list(1, Ap),
  #effects = list(data.frame(b0 = rep(1, nrow(coop))), s = indexs
  effects = list(data.frame(b0 = 1, altitude = dp[, 3],age_0 = 10,age_1 = 14,year = 2019),s = indexs


    #list(data.frame(b0 = 1), s = indexs
))

# stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)



formula <- y ~ 0 + b0 +age_0 +age_1+year+ altitude+f(s, model = spde)


res <- inla(formula,
  family = "binomial", Ntrials = numtrials,
  control.family = list(link = "logit"),
  data = inla.stack.data(stk.full),
  control.compute = list(return.marginals.predictor=TRUE,waic=TRUE),
  control.predictor = list(
    compute = TRUE, link = 1,
    A = inla.stack.A(stk.full)
  )
)


summary(res)



res$waic$waic


index <- inla.stack.index(stack = stk.full, tag = "pred")$data



prev_mean <- res$summary.fitted.values[index, "mean"]
prev_ll <- res$summary.fitted.values[index, "0.025quant"]
prev_ul <- res$summary.fitted.values[index, "0.975quant"]



r_prev_mean <- rasterize(
  x = coop, y = ra, field = prev_mean,
  fun = mean
)

pal <- colorNumeric(terrain.colors(100), c(0, 0.45), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_mean, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
    pal = pal,
    values = values(r_prev_mean), title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))

r_prev_ll <- rasterize(
  x = coop, y = ra, field = prev_ll,
  fun = mean
)
pal <- colorNumeric(terrain.colors(100), c(0, 0.2), na.color = "transparent")

leaflet() %>%
  
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_ll, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
    pal = pal,
    values = values(r_prev_ll), title = "LL"
  ) %>%
  addScaleBar(position = c("bottomleft"))


r_prev_ul <- rasterize(
  x = coop, y = ra, field = prev_ul,
  fun = mean
)
pal <- colorNumeric(terrain.colors(100), c(0, 0.9), na.color = "transparent")

leaflet() %>%
  
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_ul, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
    pal = pal,
    values = values(r_prev_ul), title = "UL"
  ) %>%
  addScaleBar(position = c("bottomleft"))


marg <- res$marginals.fitted.values[index][[1]]
1 - inla.pmarginal(q = 0.05, marginal = marg)
excprob <- sapply(res$marginals.fitted.values[index],
FUN = function(marg){1-inla.pmarginal(q = 0.05, marginal = marg)})


r_excprob <- rasterize(
  x = coop, y = ra, field = excprob,
  fun = mean
)
pal <- colorNumeric(terrain.colors(100), c(0, 1.2), na.color = "transparent")


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_excprob, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
    pal = pal,
    values = values(r_excprob), title = "P(p>0.05)"
  ) %>%
  addScaleBar(position = c("bottomleft"))




prev_mean_1_anysth = prev_mean
result = data.frame(x = dp[,1],y = dp[,2],prev_mean_sh = prev_mean_sh,prev_mean_sman=prev_mean_sman,prev_mean_anysch=prev_mean_anysch,prev_mean_asc=prev_mean_asc,prev_mean_hkw=prev_mean_hkw,prev_mean_tri=prev_mean_tri,prev_mean_anysth=prev_mean_anysth,prev_mean_1_sh=prev_mean_1_sh,prev_mean_1_sman=prev_mean_1_sman,prev_mean_1_anysch=prev_mean_1_anysch,prev_mean_1_asc=prev_mean_1_asc,prev_mean_1_hkw=prev_mean_1_hkw,prev_mean_1_tri=prev_mean_1_tri,prev_mean_1_anysth=prev_mean_1_anysth)

head(result)


 write.csv(result,"result.csv", row.names = FALSE)




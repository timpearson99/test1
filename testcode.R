
# reminders

# 1)
# lex info on 
# location allocation supply demand modelling
# seek to get 
# use tbart package for this
# https://cran.r-project.org/web/packages/tbart/index.html
# lex emaild code and info on this to me at shef


# 2)
flex dashboard
nice easy way of creating interactive web pages from r code 
# look at exaplles within rstudio - codeavailable for these


# 3)
# rstudio uses rm-markup to generate pdf/word/html document fron r code with code and mapa emberred as required and supports latex for technical text etc, all very easy & nice


# 4) books
# spatial analysis & mapping in r
# lex comber








# ---------------------------
# friday 27/05/2016
# ---------------------------
# am session, starting off with stuff in "introduction to visualising spatial data in R ....."
# Part-III: creating and manipulating spatial data

vec<-vector(mode="numeric",length=3)
df<-data.frame(x=1:3,y=c(1/2,2/3,3/4))
vec; df



library(GISTools)
mat<-as.matrix(df)
sp1<-SpatialPoints(coords=mat)
class(sp1)

spdf<-SpatialPointsDataFrame(sp1,data=df)
class(spdf)
spdf



# --------------------
# Projections
# --------------------
library(rgdal)
# install.packages("raster")

library(raster)
london <-shapefile("~/Documents/R_Leeds/Creating-maps-in-R-master/data/london_sport.shp")
london
class(london); summary(london)

proj4string(london) <-NA_character_
proj4string(london) <-NULL
?proj4string
proj4string(london) <-CRS("+init=epsg:27700")
proj4string(london)
is.projected(london)


EPSG<-make_EPSG()
EPSG[1:3,]
EPSG[grepl("WGS 84$", EPSG$note),]


lnd84<-spTransform(london,CRS("+init=epsg:4326"))
saveRDS(object=lnd84,file="data/lnd84.Rds")
rm(lnd84)




# -------------------------
# Attribute joins p13
# -------------------------
library(rgdal)
london<-readOGR(dsn="data","london_sport")
plot(london)
nrow(london)

rm(crime_data)
crime_data <-"~/Documents/R_Leeds/Creating-maps-in-R-master/data/mps-recordedcrime-borough.csv"
crime_data <-read.csv(crime_data,stringsAsFactors=F)
dim(crime_data)
crime_data[1:3,]
head(crime_data,3)
head(crime_data$CrimeType,3)


crime_theft <-crime_data[crime_data$CrimeType=="Theft & Handling",]
head(crime_theft,3)


crime_ag <-aggregate(CrimeCount ~ Borough, FUN=sum, data=crime_theft)
head(crime_ag,3)







# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# lex talk on manupulation spatial objects/data
# Rmarkdown nice tool
# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Lex's matiarials for his friday am examples/demo this on last slide of robinLL course slides
# To access the materials, please see here: https://www.dropbox.com/sh/akwx5cw611c3rz2/AAC8zgQARcdO80sJqHihnbvta?dl=0


# rgeos library has lots of gis related functions
# 3 examples intersection, buffering, pin


data()		# all datasets in R ()not all spatial)


# union - use to combine spatial objects, eg combine several polygons to produce outline

# full union is where combine spatial objects and attribute data associated with spatial objects, example of this is p6 in "manipulating spatial objects" handout notes

# load data from page-1
library(GISTools)
data(tornados)
ls()
plot(us_states)
plot(torn,add=T,cex=0.2,pch=19,col=add.alpha(brewer.pal(7,"Greys"),0.3)[4])


# page-6 full union
bb<-bbox(us_states2)
grd<-GridTopology(cellcentre.offset=c(bb[1,1]-200,bb[2,1]-200),cellsize=c(1e5,1e5),cells.dim=c(47,29))

# interpolation layer
rm(int.layer)
int.layer<-SpatialPolygonsDataFrame(as.SpatialPolygons.GridTopology(grd), data=data.frame(c(1:1363)), match.ID=F)
names(int.layer) <-"ID"
proj4string(int.layer) <-proj4string(us_states2)
proj4string(int.layer)
is.projected(int.layer)



plot(us_states2,col="gold")
plot(int.layer,add=T)


# this not quite correct still have full grid - look at this some time p6/7 handout  ***!!!!!****
int.layer <-int.layer[us_states2,]		# ignore projection warning
plot(us_states2,col="gold")
plot(int.layer,add=T)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!
# check error here??
proj4string(int.layer)
proj4string(int.layer) <-NA_character_
identical(proj4string(int.layer), proj4string(int.layer))
identical(proj4string(int.layer), proj4string(us_states2))
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!



# undertake intersection
int.res <-gIntersection(int.layer,us_states2,byid=T)

head(names(int.res))
head(data.frame(int.layer))


head(data.frame(us_states2)[,c(1,2,6:9)])
       # AREA   STATE_NAME POP1990 POP1997 POP90_SQMI HOUSEHOLDS
# 1  67286.88   Washington 4866692 5604260         72    1872431
# 2 147236.03      Montana  799065  888723          5     306163
# 3  32161.66        Maine 1227928 1244828         38     465312
# 4  70810.15 North Dakota  638800  644782          9     240878
# 5  77193.62 South Dakota  696004  736549          9     259034
# 6  97799.49      Wyoming  453588  484529          5     168839




# page-8
tmp<-strsplit(names(int.res)," ")
states.id<-(sapply(tmp,"[[",2))
intlayer.id<-(sapply(tmp,"[[",1))


# rm(states.areas)
int.areas <-gArea(int.res,byid=T)
states.areas <-gArea(us_states2,byid=T)
index<-match(states.id,row.names(us_states2))
states.areas <-states.areas[index]
states.prop <-int.areas/states.areas
df<-data.frame(intlayer.id,states.prop)
pop97<-zapsmall(us_states2$POP1997[index] * states.prop,1)


houses<-zapsmall(us_states2$POP1997[index] * states.prop,1)








# -------------------------------
# friday afternoon session
# Lex Comber - Rasters - "intro to Raster"  handout
# copy code from Lex's Raster_Analysis.Rmd code file
# -------------------------------
library(GISTools)
library(raster)
library(rgl)


data(volcano); class(volcano)
data(meuse.grid); class(meuse.grid)
ls()


x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = terrain.colors(100), 
      axes = FALSE, ylab = "", xlab = "", asp = 1)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = "peru")
box()
title(main = "Maunga Whau Volcano", font.main = 4)


# SpatialPixelsDataFrame
# use expand.grid to create coordinates
coords.xy <- expand.grid(x = x, y = y)

# then use these and as.vector to create the SPDF 
volc.spdf <- SpatialPixelsDataFrame(coords.xy, 
  data = data.frame(heigh = as.vector(volcano)))

image(volc.spdf, col = topo.colors(100), asp = 1, 
      axes = FALSE, ylab = "", xlab = "")
contour(volc.spdf, levels = seq(90, 200, by = 5),
        add = TRUE, col = "peru")





# load the meuse.grid data 
data(meuse.grid)
class(meuse.grid)
# create a SpatialPixelsDataFrame object
coordinates(meuse.grid) <- ~x+y
class(meuse.grid)
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame") 
class(meuse.grid)
# create 3 raster layers
r1 <- raster(meuse.grid, layer = 3) #dist
r2 <- raster(meuse.grid, layer = 4) #soil
r3 <- raster(meuse.grid, layer = 5) #ffreq

# With the raster layers, the usual raster operations can be undertaken. So for example, to  identify the locations that are half of the maximum distance away from the Meuse river, have a soil class of 1 (calcareous weakly developed meadow soils, light sandy clay) and have a flooding frequency class of 3, (once in 50-years). The following logical operations can be used to do this:
r1.1 <- r1 > 0.5 
r2.1 <- r2 >= 2 
r3.1 <- r3 < 3


# page-4 result of a raster calculator operation
result <- r1.1 * r2.1 * r3.1 
table(as.vector(result$layer))
image(result, asp=1, axes=FALSE, ylab = "", xlab = "")
contour(result, levels = c(0,1), add = TRUE, col = "black", lwd = 2)
title("The result of a raster calculator operation")



# page-4/5 aside on 3D plotting
install.packages("rgl")
library(rgl)
z <- 2 * volcano # Exaggerate the relief
x <- 10 * (1:nrow(z)) # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z)) # 10 meter spacing (E to W)
zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen,alpha=0) # height color lookup table
col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
open3d()
rgl.surface(x, y, z, color=col, alpha=0.75, back="lines")



# page-5
# The code below adds another surface to the graphic as shown in the next figure:
colorlut <- heat.colors(zlen,alpha=1) # use different colors for the contour map
col <- colorlut[ z-zlim[1]+1 ] 
rgl.surface(x, y, matrix(1, nrow(z), ncol(z)),color=col, back="fill")







# Vector and Raster Conversion #
# The `raster` package contains  functions for converting from vector to raster formats: `rasterToPolygons` which converts to a `SpatialPolygonsDataFrame` object, and `rasterToPoints` which converts to a `matrix` object. These are illustrated in the code below. Notice how the original raster imposes a grid structure on the polygons that are created.

## Polygons and Raster ##
The US states data can be converted to a raster:
# load some data and convert to raster
data(tornados)
# set up the raster, r
r <- raster(nrow = 60 , ncols = 120, ext = extent(us_states)) 
# convert polygons to raster, using an attribute
r <- rasterize(us_states, r, "STATE_FIPS")
plot(r)



# These can be converted back to polygons:
plot(r)
poly1 <- rasterToPolygons(r, dissolve = T)
plot(poly1, add = T)






## Points and Raster
The tornado point data can be converted to a raster: 
# set up the raster, r
r = raster(nrow = 180, ncols = 360, 
  ext = extent(us_states)) 
# create a SpatialPoints variable
t2 <- as(torn, "SpatialPoints")
# define the raster 
# NOTE the function to count the points in each cell
r <- rasterize(t2, r, fun=sum)
# set the plot extent by specifying the plot colour 'white'
plot(r, col = "white")
plot(us_states, add = T, border = "grey") 
plot(r, add = T)




# These can be converted back to points
points1 <- rasterToPoints(r)
p <- as(r, 'SpatialPointsDataFrame')
plot(p, col = "#FB6A4A4C", cex = 0.5)
g <- as(r, 'SpatialGridDataFrame')
x <- as(r, 'SpatialPixelsDataFrame')



# And to illustrate the conversion the points, rasterized polygons and original polygons can be plotted: 
plot(points1, col = "grey", axes = FALSE, xaxt='n', cex = 0.7, asp = 1)
plot(us_states, border = "red", add = T, lwd = 2) 
plot(poly1, lwd = 1.5, add = T) 





# --------------------------------------
# fri-late afternoon final session
# lex comber
# point pattern analysis handout
# --------------------------------------


library(GISTools)
# load the data
data(newhaven)
# 1. Have a look at breach
class(breach)
plot(tracts)
plot(breach, add = T)


# KERNAL DENSITY ESTIMATION
# 2. Calculate the KDE
breach.dens <- kde.points(breach,lims=tracts) 
# 3. Plot the KDE
# with a level plot 
level.plot(breach.dens)
# 4. Clip the plot
# use the 'masking' function 
masker <- poly.outer(breach.dens,tracts,extend=100) 
add.masking(masker)
# Add the census tracts 
plot(tracts,add=TRUE)







# page-3: INTERPOLATION
## IDWInverse distance weighting (IDW) estimate the value of *z* at location *x* using a weighted mean of nearby observations. It assumes that observations of *z* at points closer to *x* should be given more importance in the interpolation and greater weight is given to these points.

# The `muese` data will be used which has data recording zinc levels, and the aim is to interpolate this to get values for each cell in the `meuse.grid` dataset. It might be useful to re-familiarise yourself with these 2 datasets:


# First, let's look at `meuse`:
library(sp)
library(gstat)
data(meuse)
head(meuse)
dim(meuse)
# only 155 data points
# conver to SPDF
coordinates(meuse) <- ~x+y
plot(meuse)



# Second, now let's examine `muese.grid`:
data(meuse.grid)
head(meuse.grid)
dim(meuse.grid)
# 3103 data points
# conver to SPDF
coordinates(meuse.grid) <- ~x+y
plot(meuse.grid)


# The `gstat` package has a function called `idw` that performs inverse distance weighting. You should load this package and examine this function:
# Now apply this function to generate a surface of zinc and p[lot the results:
install.packages("gstat")
library(gstat)
zinc.idw = idw(zinc~1, meuse, meuse.grid)
# create a pixel surfacce 
tmp <- as(zinc.idw, "SpatialPixelsDataFrame")
spplot(tmp["var1.pred"], main = "zinc IDW interpolation")





# page-6
# Kriging - The data values produced by the IDW interpolation always passes exactly through uniquely located measurement points. If the data are the result of very reliable measurement, and the underlying process is largely deterministic, this is fine. However, if the process is subject to random errors in measurement or sampling, or the underlying process is stochastic, there will be a degree of random variability in the observed values. In kriging, the observed quantity *zi* is modelled to be the outcome of a random process composed of *f(xi)*, a deterministic trend function, *v(xi)*, a random error of observation associated with the measurement or sampling error at the point *xi* that is assumed to have a Gaussian distribution with mean zero and variance of 2 standard deviations. This is sometimes called the *nugget* effect, reflecting the fact that kriging was initially applied in gold mining to estimate mineral concentration. However, although this was modelled as a continuous quantity, in reality minerals such as gold occur in small nuggets and exploratory mining samples taken at certain locations would be subject to highly localised variability, depending on whether or not a nugget was discovered. 


evgm <-variogram(zinc~1,meuse)
lzn.fit = fit.variogram(evgm, model = vgm(150000, "Sph", 900, 1))
lzn.fit
plot(evgm, lzn.fit)

# If you are happy with the model fit to the semi-variance then the model can be used to 'krig' the data:
lzn.kriged = krige(zinc~1, meuse, meuse.grid, model = lzn.fit)
tmp <- as(lzn.kriged, "SpatialPixelsDataFrame")
spplot(tmp["var1.pred"], main = "zinc Krige interpolation")



# page-8: It also possible to examine the variance of the interpolated results (for both the kriged and IDW approaches: 
spplot(tmp["var1.var"], main = "zinc Krige variance")





# ---------------------------------------
# final-final friday pm session
# robin lovelace spatio-temporal data in R
# ---------------------------------------
# pdf and tutorial for this is here:
# ~/Documents/R_Leeds/LexCombes_Data_Code_friday/rmaterials-lc/spatio-temporal-data.pdf


# this is a very quick overview of working with space-time data within the spacetime package - useful to know about, quite complex statistics
vignette(package="spacetime")
data(package="spacetime")
ls()

vignette("jss816")







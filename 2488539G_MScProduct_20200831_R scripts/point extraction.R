setwd('D:/uni/project MSc/Patia/mosaic')
library(raster)
library(rgdal)
#library(sf)
library(sp)
library(ggplot2)


point <- readOGR(dsn=path.expand("D:/uni/project MSc/Patia/mosaic"),
                            layer="Patiapoints")
head(point)
class(point)
crs(point)
extent(point)
point

plot(point, col="cyan1", lwd=3,add = TRUE,
     main="Patia Plot")


###############NDVI###############
#open your raster 
r<- raster("20180918_Patia_mosaic.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
ndvi=cbind(point,rasValue)
write.table(ndvi,file='ndvi.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
ndvi

##############NIR#########
r<- raster("NIRRaster.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
nir=cbind(point,rasValue)
write.table(nir,file='nir.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
nir


############RED##############
r<- raster("RedRaster.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
red=cbind(point,rasValue)
write.table(red,file='red.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
red

##################blue#########
r<- raster("BlueRaster.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
blue=cbind(point,rasValue)
write.table(blue,file='blue.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
blue

#################green#############
r<- raster("GreenRaster.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
green=cbind(point,rasValue)
write.table(green,file='green.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
green

##############msavi2#########
r<- raster("MSAVI2.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
msavi2=cbind(point,rasValue)
write.table(msavi2,file='MSAVI2.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
msavi2

################savi#########
r<- raster("SAVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
savi=cbind(point,rasValue)
write.table(savi,file='SAVI.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
savi

###########EVI#########
r<- raster("EVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
evi=cbind(point,rasValue)
write.table(evi,file='EVI.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
evi

###############dvi########
r<- raster("DVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
dvi=cbind(point,rasValue)
write.table(dvi,file='DVI.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
dvi

############LAI########
r<- raster("LAI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
lai=cbind(point,rasValue)
write.table(lai,file='lai.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
lai

##########VARI##########
r<- raster("VARI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
vari=cbind(point,rasValue)
write.table(vari,file='vari.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
vari

###############RDVI#########
r<- raster("RDVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
rdvi=cbind(point,rasValue)
write.table(rdvi,file='rdvi.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
rdvi

##############NLI#########
r<- raster("NLI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
nli=cbind(point,rasValue)
write.table(nli,file='NLI.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
nli

################MSR#####
r<- raster("MSR.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
msr=cbind(point,rasValue)
write.table(msr,file='msr.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
msr

#################GOSAVI#####
r<- raster("GOSAVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
gosavi=cbind(point,rasValue)
write.table(gosavi,file='GOSAVI.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
gosavi

############gli#######
r<- raster("GLI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
gli=cbind(point,rasValue)
write.table(gli,file='gli.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
gli

##############gndvi##########
r<- raster("GNDVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
gndvi=cbind(point,rasValue)
write.table(gndvi,file='GNDVI.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
gndvi

##################grvi######
r<- raster("GRVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
grvi=cbind(point,rasValue)
write.table(grvi,file='GRVI.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
grvi


###########SR NIRB###########
r<- raster("NIRB.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
nirb=cbind(point,rasValue)
write.table(nirb,file='nirb.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
nirb

###############RNIR#########
r<- raster("RNIR.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
rnir=cbind(point,rasValue)
write.table(rnir,file='rnir.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
rnir

##################RG#########
r<- raster("RG.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
rg=cbind(point,rasValue)
write.table(rg,file='RG.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
rg

##################RB##########
r<- raster("RB.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
rb=cbind(point,rasValue)
write.table(rb,file='RB.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
rb

################GNIR##########
r<- raster("GNIR.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
gnir=cbind(point,rasValue)
write.table(gnir,file='gnir.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
gnir

#############GR###########
r<- raster("GR.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
gr=cbind(point,rasValue)
write.table(gr,file='GR.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
gr

##############GB##########
r<- raster("GB.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
gb=cbind(point,rasValue)
write.table(gb,file='gb.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
gb

##############BNIR#######
r<- raster("BNIR.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
bnir=cbind(point,rasValue)
write.table(bnir,file='BNIR.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
bnir

##################BR##########
r<- raster("BR.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
br=cbind(point,rasValue)
write.table(br,file='BR.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
br

###############bg#########
r<- raster("BG.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
bg=cbind(point,rasValue)
write.table(bg,file='BG.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
bg

##############IPVI#########
r<- raster("IPVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
ipvi=cbind(point,rasValue)
write.table(ipvi,file='ipvi.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
ipvi

#############rvi##############
r<- raster("RVI.tif")  
crs(r)
r

rasValue=extract(r, point)
rasValue
rvi=cbind(point,rasValue)
write.table(rvi,file='rvi.csv', 
            append=FALSE, sep= ",", row.names = FALSE, 
            col.names=TRUE)
rvi


##################arvi##########
setwd('D:/uni/project MSc/Patia/mosaic')
library(raster)
library(rgdal)
library(sf)

bands<- stack("20180918_Patia_mosaic.tif")       
#plotRGB(bands, 3,2,1, stretch= "lin")
bands
nlayers(bands)


blue<- bands[[1]]
green<- bands[[2]]
red<- bands[[3]]
nir<- bands[[4]]

names(bands)<- c("Blue", "Green", "Red", "NIR")
names(bands)


plot(red, main= 'Red- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
head(red)
writeRaster(red, filename = paste0('Red', '.tif'),
            overwrite=TRUE)
dev.off()

#NDVI
ndvi <- (nir-red)/(nir+red)
plot(ndvi, main= 'NDVI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
head(ndvi)
# Write to disk
writeRaster(ndvi, filename = paste0('NDVI', '.tif'),
            overwrite=TRUE)
dev.off()


# NDVI

# Created a VI function (vegetation index)
#VI <- function(img, k, i) {
   #     bk <- img[[k]]
  #      bi <- img[[i]]
    #    vi <- (bk - bi) / (bk + bi)
   #     return(vi)
#}

#ndvi <- VI(bands, 4, 3)
# 4 and 3 refer to the bands we'll use
#
#plot(ndvi, col = rev(terrain.colors(10)), main = "PS-NDVI",
 #    cex.lab= 0.7, cex.axis= 0.7, cex.main= 1)
#dev.off()






#SAVI <- function(img, k, i) {
 #       bk <- img[[k]]
  #      bi <- img[[i]]
   #     savi <- (1.5*(bk - bi)) / (bk + bi +0.5)
    #    return(savi)
#}

#savi <- SAVI(bands, 4, 3)
#plot(savi, col = rev(terrain.colors(10)), main = "PS-SAVI",
 #    cex.lab= 0.7, cex.axis= 0.7, cex.main= 1)
#dev.off()

#SAVI
c1<- 1.5; C2<- .5
savi <- 1.5*(nir-red)/(nir+red+0.5)
plot(savi, main= 'SAVI-PS', cex.lab= 0.7, cex.axis=0.7, 
     cex.main= 1)
writeRaster(savi, filename = paste0('SAVI', '.tif'),
            overwrite=TRUE)
dev.off()


#MSAVI <- function(img, k, i) {
 #       bk <- img[[k]]
 #       bi <- img[[i]]
  #      msavi <- 2*bk +1 -sqrt((2* bk +1)^2 -8*(bk - bi))/2
  #      return(msavi)
#}

#msavi <- MSAVI(bands, 4, 3)
#plot(msavi, col = rev(terrain.colors(10)), main = "PS-MSAVI",
 #    cex.lab= 0.7, cex.axis= 0.7, cex.main= 1)
#dev.off()


#MSAVI2
msavi2<- (2*nir + 1 - sqrt((2*nir+1)^2 - 8*(nir -red)))/2
plot(msavi2, main= 'MSAVI2- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(msavi2, filename = paste0('MSAVI2', '.tif'),
            overwrite=TRUE)
dev.off()

#RVI
rvi<- (nir/ red)
plot(rvi, main= 'RVI- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(rvi, filename = paste0('RVI', '.tif'),
            overwrite=TRUE)
dev.off()

#ARVI
arvi<- (nir- (red +1* (blue-red)))/ (nir + (red + 1*(blue-red)))
plot(arvi, main= 'ARVI- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(arvi, filename = paste0('ARVI', '.tif'),
            overwrite=TRUE)
dev.off()

#IPVI
ipvi<- nir/ (nir+red)
plot(ipvi, main= 'IPVI- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(ipvi, filename = paste0('IPVI', '.tif'),
            overwrite=TRUE)
dev.off()

#DVI
dvi<- nir- red
plot(dvi, main= 'DVI- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(dvi, filename = paste0('DVI', '.tif'),
            overwrite=TRUE)
dev.off()

#Simple Ratios
sr1<- blue/green
plot(sr1, main= 'B/G', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr1, filename = paste0('BG', '.tif'),
            overwrite=TRUE)
dev.off()

sr2<- blue/red
plot(sr2, main= 'SRBR- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr2, filename = paste0('BR', '.tif'),
            overwrite=TRUE)
dev.off()

sr3<- blue/nir
plot(sr3, main= 'SRBNIR- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr3, filename = paste0('BNIR', '.tif'),
            overwrite=TRUE)
dev.off()

sr4<- green/blue
plot(sr4, main= 'SRGB- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr4, filename = paste0('GB', '.tif'),
            overwrite=TRUE)
dev.off()

sr5<- green/red
plot(sr5, main= 'SRGR- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr5, filename = paste0('GR', '.tif'),
            overwrite=TRUE)
dev.off()

sr6<- green/nir
plot(sr6, main= 'SRGNIR- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr6, filename = paste0('GNIR', '.tif'),
            overwrite=TRUE)
dev.off()

sr7<- red/blue
plot(sr7, main= 'SRRB- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr7, filename = paste0('RB', '.tif'),
            overwrite=TRUE)
dev.off()

sr8<- red/green
plot(sr8, main= 'SRRG- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr8, filename = paste0('RG', '.tif'),
            overwrite=TRUE)
dev.off()

sr9<- red/nir
plot(sr9, main= 'SRRNIR- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr9, filename = paste0('RNIR', '.tif'),
            overwrite=TRUE)
dev.off()

sr10<- nir/blue
plot(sr10, main= 'SRNIRB- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(sr10, filename = paste0('NIRB', '.tif'),
            overwrite=TRUE)
dev.off()

#GRVI
grvi<- (nir/ green)
plot(grvi, main='GRVI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main=1)
writeRaster(grvi, filename = paste0('GRVI', '.tif'),
            overwrite=TRUE)
dev.off()

#EVI
c1<- 6; C2<- 7.5; l<-1; G<- 2.5
evi <- 2.5*(nir-red)/(nir+ 6*red + 7.5*blue +1)
plot(evi, main= 'EVI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)

writeRaster(evi, filename = paste0('EVI', '.tif'),
            overwrite=TRUE)
dev.off()

##LAI
C1<- 3.618; C2<- 0.118
lai<- (C1* evi - C2)
plot(lai, main='LAI- PS', cex.lab= 0.7, cex.axis=0.7, 
     cex.main= 1)
writeRaster(lai, filename = paste0('LAI', '.tif'),
            overwrite=TRUE)
dev.off()

#GNDVI
gndvi<- (nir-green)/(nir+green)
plot(gndvi,  main= 'GNDVI- PS', cex.lab=0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(gndvi, filename = paste0('GNDVI', '.tif'),
            overwrite=TRUE)
dev.off()

#GLI
gli<- ((green-red)+ (green-blue))/ ((2*green)+ red+ blue)
plot(gli, main= 'GLI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(gli, filename = paste0('GLI', '.tif'),
            overwrite=TRUE)
dev.off()

#GOSAVI
d<- 0.16
gosavi<- (nir-green)/ (nir +green+ d)
plot(gosavi, main= 'GOSAVI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(gosavi, filename = paste0('GOSAVI', '.tif'),
            overwrite=TRUE)
dev.off()

#MSR
msr<- ((nir/red)- 1)/ (sqrt(nir/red)+ 1)
plot(msr, main= 'MSR- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(msr, filename = paste0('MSR', '.tif'),
            overwrite=TRUE)
dev.off()

#NLI
nli<- (nir^2 - red)/ (nir^2 + red)
plot(nli, main= 'NLI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(nli, filename = paste0('NLI', '.tif'),
            overwrite=TRUE)
dev.off()

#VARI
vari<- (green- red)/(green+red)
plot(vari, main= 'VARI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(vari, filename = paste0('VARI', '.tif'),
            overwrite=TRUE)
dev.off()

#RDVI
rdvi<- (nir-red)/(nir+red)^0.5
plot(rdvi, main= 'RDVI- PS', cex.lab= 0.7, cex.axis= 0.7,
     cex.main= 1)
writeRaster(rdvi, filename = paste0('RDVI', '.tif'),
            overwrite=TRUE)
dev.off()

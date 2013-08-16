facePath<-"e:/Pvle_proj/pvle/demo"
faceProjFolder<-paste(facePath,"/FaceProj",sep="")

unlink(faceProjFolder,recursive=TRUE)
system.time(faceProj<-PvleProj("FaceProj",facePath,"FaceProj"))

#Create mask array from the image files.
files <- dir(paste(facePath,"/rawImgs",sep=""), pattern = "*.img", full.names= TRUE)
imageDim <- f.read.analyze.header(files[1])$dim[2 : 4]
mask3D <- array(1, imageDim)

files<-files
for (file in files)
{
	img <- f.read.analyze.volume(file)[,,,1]
	mask3D <- mask3D * ( !is.na(img) )
}
faceProj<-LoadArrayMask(faceProj,mask3D)

#Load image files
for (file in files)
	faceProj<-AddImageFile(faceProj,file)



interceptModel<-function(y,show) #Return a vector of coefficient estimate, standard error, t value, and p value
	{
		return(coef(summary(lm(formula=y~1))))
	}

system.time(testRst<-RunPvle(faceProj,interceptModel,8))




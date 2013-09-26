works_with_R("3.0.1",maps="2.3.2",hexbin="1.26.2",maptools="0.8.27",
             sp="1.0.13",mapproj="1.2.1")
 
# piRate the data from the militaRy
download.file("http://msi.nga.mil/MSISiteContent/StaticFiles/Files/ASAM_shp.zip", destfile="ASAM_shp.zip")
unzip("ASAM_shp.zip")
 
# extRact the data fRame we need fRom the shape file
pirates <- as.data.frame(readShapePoints("ASAM 19 SEP 13")) # you may need to use a diffeRent name depending on d/l date
 
save(pirates, file="../data/pirates.RData")

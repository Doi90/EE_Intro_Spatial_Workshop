library(rgdal)
library(raster)

# raster part
dem <- raster("9sec_dem")
# display a summary of the raster (to check it has loaded)
dem

# inspect:
res(dem) # returns the cell size dimensions (an x and y value)
ncol(dem) # the number of columns
nrow(dem) # the number of rows
ncell(dem) # the number of cells (rows * columns)
dim(dem) # the dimensions of the raster (number of columns, number of rows, number of bands)
projection(dem): # the projection of the raster
  xmin(dem) # the x and y minimum and maximum coordinates
ymin(dem) 
xmax(dem) 
ymax(dem)
range(values(dem), na.rm=T) # the range of raster values, ignoring NoData
cellStats(dem, mean) # the mean, min, max, sd, sum
freq(dem) # for categorical rasters, the frequency of values

# compare and combine rasters
dem2 <- raster("3sec_dem")
# display a summary of the raster (to check it has loaded)
dem2
# what is the difference?

# in order to use rasters together, they should have the same resolution, so that all cells align
# resample the coarser one to the finer one
dem9_fine <- resample
# challenge 1: what is the difference between the 2 elevation datasets?
dem_dif <- dem9_fine - dem2
# creating slope
slope <- terrain(dem2, opt="slope", file="australia_slope.tif", unit="degrees") # for use in the challenge later)
# challenge 2: create 3 categories of slope: flat, moderate, steep
# create an empty matrix with the required dimensions
rclm <- matrix(0, nrow=3, ncol=3)
# populate each row of the matrix with reclassification information
rclm[,1] <- seq(xmin,ymin,zmin) # have to find out what a good category break would be 
rclm[,2] <- seq(xmax,ymax,zmax)
rclm[,3] <- c(1:3)
# could also create random points to sample? and then create also random points in the clipped
# DEM to compare a boxplot in the challenge?
slope3 <- reclassify(slope, rclm, right=FALSE)

# check it out!
plot(slope3)


# 6.3 Clipping rasters with polygons  --- probably already for the analysis part?
# Clipping a raster to polygons from a vector data set is a two step process. First, the
# raster gets cropped to the rectangular extent of the polygons (crop function), and then all the
# cells that fall outside of the polygons are set to NoData (mask function).

# load vector data
pa <- readOGR("CAPAD.shp")
# if necessary, reproject
dem_prj <- spTransform(dem, CRS=crs(pa))
# first crop using extent from the polygon dataset
dem_pa <- crop(dem, pa)
dem_pa <- crop(dem, extent(pa)) # is this doing the same?
# now mask the cells that do not overlap a polygon
dem_pa_mask <- rasterize(pa, dem_pa, mask=TRUE)
# plot the clipped raster
plot(dem_pa_mask)
#ignore NA in calculations with na.rm=TRUE

# inspect the summary statistics for values within the protected areas
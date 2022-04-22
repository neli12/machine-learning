### Script for mapping clusters using raster maps

## Load libraries
library(raster)
library(cluster)

## Set directory
dir.imagens.base <- "C:/Users"

# Create a list with your raster names
imagens <- list.files(path = dir.imagens.base, pattern = "map.tif", full.names = TRUE)
imagens

# Create an empty list to save your raster layers and convert character into raster
covs <- list()
for (i in 1:length(images)){
  covs[i] <- raster(images)
}

# Create a raster stack from your list of raster
covs.stack <- stack(covs)

# Get the values of the raster stack
stack_vals <- getValues(covs.stack)
summary(stack_vals)

# Elbow method to check the optimal number of cluster
k.max <- 20
data <- (na.omit(scale(stack_vals))) #scale and exclude NA's
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(wss)



### Clustering
# Use the clara function from cluster package. In this code, a sample of 1000 and the euclidean distance are used.
# I used k=4, as example
clus <- cluster::clara(data, k=4, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T)  
summary(clus)


# Create am index
idx <- 1:ncell(covs.stack)
idx <- idx[-unique(which(is.na(stack_vals), arr.ind=TRUE)[,1])] 
str(idx)

# Create an empty raster using the first raster of your stack
clust_raster <- covs.stack[[1]]
plot(clust_raster)
clust_raster[] <- NA

# Transfer the clustering results to your empty raster and save it
clust_raster[idx] <- clus$clustering
plot(clust_raster) 
crs(clust_raster)

writeRaster(clust_raster, "Clusters", format = "GTiff", datatype = "FLT4S", overwrite = T)



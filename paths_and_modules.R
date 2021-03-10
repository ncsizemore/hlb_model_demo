path <- getwd()
## make a list of directory paths
simPaths <- list(cachePath = paste0(path,"/cache/"),
                 inputPath = paste0(path,"/inputs/"),
                 modulePath = paste0(path,"/modules/"),
                 outputPath = paste0(path,"/outputs/"))

setPaths(cachePath = paste0(path,"/cache/"))  ## sets custom cachePath with other paths default
setPaths(inputPath = paste0(path,"/inputs/"))  ## sets custom inputPath with other paths default
setPaths(modulePath = paste0(path,"/modules/")) ## sets custom modulePath with other paths default
setPaths(outputPath = paste0(path,"/outputs/")) ## sets custom outputPath with other paths default

# list the modules to use
simModules <- list("UpdateParameters", "CitrusInfection", "Eggs", "PsyllidSpread", "PlantSales", "TruckDistribution", "ControlMeasures")#, "CitrusInfection", "ControlMeasures", "PsyllidSpread", "PlantSales", "TruckDistribution", "Wind")

# create modules (if they don't exist yet)
invisible(lapply(simModules, function(x){if(!dir.exists(file.path(getPaths()$modulePath, x))){newModule(name = x, path = getPaths()$modulePath)}}))
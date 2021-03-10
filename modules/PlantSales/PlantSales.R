## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "PlantSales",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0", PlantSales = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "PlantSales.Rmd")),
  reqdPkgs = list("tensorflow", "tfprobability", "terra"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("activate_sales_time_step", "numeric", 1, NA, NA,
                    "Initial time to sell infected plants locally."),
    defineParameter("activate_sales_initial_time", "numeric", 1, NA, NA,
                    "Time step for local sales process of infected plants"),
    defineParameter("activate_sales_threshold", "numeric", 100L, NA, NA,
                    "number of infected psyllids in some homestead cell for infected sales to begin"),
    defineParameter("distr_sales_initial_time", "numeric", 0, NA, NA,
                    "Initial time for sales process to distribution centers.
                    This gets dynamically updated when psyllids reach homestead,
                    based on activate_sales_threshold."),
    defineParameter("distr_sales_time_step", "numeric", 1, NA, NA,
                    "Time step for sales process to distribution centers"),
    defineParameter("distr_sales_lambda", "numeric", NA, NA, NA,
                    "Intensity rate for sales to distribution centers"),
    defineParameter("distr_inf_threshold", "numeric", 1, NA, NA,
                    "Number of sales a distribution center receives before becoming infected."),
    defineParameter("psyllids_per_plant", "numeric", 10, NA, NA,
                    "Number of psyllids on each of uninfected and infected plants sold to dgs."),
    defineParameter("local_sales_time_step", "numeric", 1, NA, NA,
                    "Time step for local sales process of infected plants"),
    defineParameter("local_sales_intensity", "numeric", 10^4, NA, NA,
                    "Intensity of local sales process k*lambda"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = dplyr::bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("maps", objectClass = "list", desc = "list of various map params. ones beginning with . are updated; others are fixed.", sourceURL = NA),
    # expectsInput("uninfected", objectClass = "VeloxRaster", desc = "a", sourceURL = NA),
    # expectsInput("infected", objectClass = "VeloxRaster", desc = "a", sourceURL = NA),
    # expectsInput(objectName = "dgs", objectClass = "sf", desc = "sf data frame containing discount garden stores, probabilities, their local region, and tracks sales", sourceURL = NA),
    # expectsInput(objectName = "infected_sales", objectClass = "VeloxRaster", desc = "Raster containing the number of infected sales received at each square", sourceURL = NA)
  ),
  outputObjects = dplyr::bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dgs", objectClass = "sf", desc = "sf data frame containing discount garden stores, probabilities, their local region, and tracks sales")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.PlantSales = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$activate_sales_initial_time, "PlantSales", "activate_sales")
    },
    activate_sales = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- ActivateSales(sim)
      
      # schedule future event(s)

     #if (time(sim) == 5){
     if (time(sim) == params(sim)$distr_sales_initial_time - 1){
      sim <- scheduleEvent(sim, params(sim)$distr_sales_initial_time, "PlantSales", "allocate_sales")
      sim <- scheduleEvent(sim, params(sim)$local_sales_initial_time, "PlantSales", "local_sales")
      } else{
        sim <- scheduleEvent(sim, time(sim) + P(sim)$activate_sales_time_step, "PlantSales", "activate_sales")
      }
      # ! ----- STOP EDITING ----- ! #
    },
    allocate_sales = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- AllocateSales(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$distr_sales_time_step, "PlantSales", "allocate_sales")
      # ! ----- STOP EDITING ----- ! #
    },
    local_sales = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- LocalSales(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$local_sales_time_step, "PlantSales", "local_sales")
      sim <- scheduleEvent(sim, time(sim), "PlantSales", "assess_local_sales")
      
      # ! ----- STOP EDITING ----- ! #
    },
    assess_local_sales = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      sim <- AssessLocalSales(sim)
      
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  # let psyllids survive on dgs squares
  #browser()
  sim$maps$tensors$pr_psyllid_survival$assign(
    tf$where(
        tf$expand_dims(tf$expand_dims(
          tf$not_equal(sim$maps$dgs$rates, 0),0L),-1L), 
      tf$constant(P(sim, "UpdateParameters")$pr_psyllid_survival_init$uninf["urban"], 
                  dtype=sim$maps$tensors$pr_psyllid_survival$dtype),
      sim$maps$tensors$pr_psyllid_survival))
  
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

ActivateSales <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  #browser()
  
  initial_times <- .activate_sales(sim$adults, time(sim), sim$maps$homestead_T, P(sim))
  #browser()
  
  
  sim$adults$assign(initial_times[[1]])
  params(sim)$distr_sales_initial_time <- initial_times[[2]]
  params(sim)$local_sales_initial_time <- initial_times[[3]]
  
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

### allocate sales from source (Homestead) to local distribution centers
AllocateSales <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  #browser()


  out <- .allocate_sales_tf(sim$adults, sim$maps$dgs$rates, sim$maps$dgs$infected_stores, P(sim)$psyllids_per_plant)

  sim$adults$assign(out[[1]])
  sim$maps$dgs$infected_stores$assign(out[[2]])


 
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### sell plants locally
LocalSales <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  # browser()
  # sim$infected_sales <- .local_sales(sim$dgs, sim$uninfected, sim$infected, sim$infected_sales, c(sim$maps,G(sim),P(sim)))
  # 
  sim$adults$assign(.local_sales(sim$adults, sim$maps$dgs, sim))
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

AssessLocalSales <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # 
  # tmp <- .assess_local_sales(sim$infected_sales, sim$inf_ind_sales, c(sim$maps,G(sim),P(sim)))
  # sim$inf_ind_sales <- tmp[[1]]
  # sim$maps$landls2 <- tmp[[2]]
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  
  dgs <- readRDS(paste0(simPaths$inputPath, "dgs.rds"))
  dgs_ras <- sim$maps$land; dgs_ras[dgs$cell] <- 1
  dgs_prob <- sim$maps$land
  dgs_summary <- dgs[,c(-1,-5)] %>% dplyr::group_by(cell) %>% dplyr::summarise(prob = sum(pop_prob))
  dgs_prob[dgs_summary$cell] <-dgs_summary$prob
  dgs_rates <- P(sim)$distr_sales_lambda * dgs_prob
  dgs_local_rates <- 0.5 * dgs_rates # just an example
  
 # browser()
  
  dgs_customer_distr <- readRDS(paste0(paths(sim)$inputPath, "/dgs_customer_distr.rds"))
  dgs_customer_distr_stack <- tf$stack(lapply(dgs_customer_distr, function(x){tf$constant(x, dtype = sim$adults$dtype)}), axis = 0L)
  
  sim$maps$dgs <- list("indicator" = sim$.mods$UpdateParameters$ras_to_tens(dgs_ras), "summary" = dgs_summary, "rates" = sim$.mods$UpdateParameters$ras_to_tens(dgs_rates), "local_rate" = sim$.mods$UpdateParameters$ras_to_tens(dgs_local_rates), "prob" = sim$.mods$UpdateParameters$ras_to_tens(dgs_prob), "infected_stores" = tf$Variable(tf$zeros_like(sim$maps$tensors$citrus)), "customer_distr" = dgs_customer_distr_stack)
  
  
  
  # dgs_customer_distr <- lapply(1:length(dgs_summary$cell), function(x){
  #   print(x)
  #   return(sim$.mods$UpdateParameters$prob_mat(dgs_summary$cell[x]))
  # })
  # saveRDS(dgs_customer_distr, paste0(paths(sim)$inputPath, "/dgs_customer_distr.rds"))
  
  
  # as.matrix(dgs_customer_distr_stack)
  # dgs_customer_distr_stack <- tf$stack(dgs_customer_distr, axis=0L)
  # 
  # chk <- tf$train$Checkpoint(a = tf$Variable(dgs_customer_distr_stack))
  # 
  # c
  # dgs
  # sim$plot(sim$.mods$UpdateParameters$tens_to_ras(chk$a[1,,]))
  # 
  # tf$train$load_variable(checkpoint_path, "a")
  # 
  # checkpoint_path = './ckpt/'
  # chk$save('./ckpt/')
  # chk$restore(tf$train$latest_checkpoint(checkpoint_path))
  # 
  # dgs_customer_distr_stack <- chk$a
  # 
  # check <- tf$train$load_checkpoint(checkpoint_path)
  # 
  # s <- tf$compat$v1$train$Saver(var_list = list("dgs_customer_distr_stack" = tf$Variable(dgs_customer_distr_stack)))
  # s$save(sess = NULL, save_path = checkpoint_path)
  # s$restore(sess = NULL, save_path = checkpoint_path)
  # t <- tf$compat$v1$train$Saver(var_list = list("dgs_customer_distr_stack"))
  # 
  # t$restore(sess = NULL, save_path = checkpoint_path)
  # 
  # a <- tf$train$Checkpoint(d = tf$Variable(tf$zeros_like(chk$a)))
  # a$restore(tf$train$latest_checkpoint(checkpoint_path))
  #
  #sim$infected_sales <- readRDS(paste0(simPaths$inputPath, "land0na.rds"))
  
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above

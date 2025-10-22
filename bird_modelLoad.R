## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "bird_modelLoad",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Sourav", ""), family = "Das", role = c("aut", "cre"), email = "souravdron@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(bird_modelLoad = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "bird_modelLoad.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("modelfolderURL", "character", NA, NA, NA, "Google Drive folder URL where model files are stored")
  ),
  inputObjects = bindrows(
    expectsInput("studyArea", "SpatVector", "Study area polygon with BCR code passed from Bird_dataPrep module"),
    expectsInput("modelfolderURL", "character", "Google Drive folder URL for species model files")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

doEvent.bird_modelLoad = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "bird_modelLoad", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "bird_modelLoad", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      plotFun(sim) # example of a plotting function
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "bird_modelLoad", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "bird_modelLoad", "save")
      
      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "bird_modelLoad", "templateEvent")
      
      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      
      # ! ----- STOP EDITING ----- ! #
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  downloadBCRModels <- function(studyArea, modelFolderURL, destinationPath ) {
    require(reproducible)
    require(googledrive)
    
    bcr_code <- unique(studyArea$subUnit)
    if (length(bcr_code) != 1) stop("studyArea must have a single BCR subUnit.")
    bcr_pattern <- paste0("_can", bcr_code, "\\.Rdata$")
    
    folder_id <- googledrive::as_id(modelFolderURL)
    files <- googledrive::drive_ls(folder_id, recursive = TRUE)
    model_files <- files[grepl(bcr_pattern, files$name), ]
   
    if (nrow(model_files) == 0) stop("No model files found for BCR: ", bcr_code)
    model_files <- head(arrange(model_files, name), 6)  # Test run: only first two models, can comment this out for full folder/file download
    
    downloaded <- character(0)
    for (i in seq_len(nrow(model_files))) {
      message(" Downloading model for: ", model_files$name[i])
      local_path <- prepInputs(
        url = model_files$id[i],
        destinationPath = destinationPath,
        targetFile = model_files$name[i],
        fun = NA, #"load",
        overwrite = TRUE
      )
      downloaded <- c(downloaded, local_path)
    }
    
    message("Downloaded ", length(downloaded), " model(s) to: ", destinationPath)
    return(downloaded)
  }
  
  # Call the function
  
  sim$modelFiles <- downloadBCRModels(
    studyArea = sim$studyArea,
    modelFolderURL = sim$modelfolderURL,
    destinationPath = file.path(inputPath(sim))#, "model/"
  )
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn) # needs ggplot2
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("modelfolderURL", sim)) {
    sim$modelfolderURL <- "https://drive.google.com/drive/u/0/folders/1o8VCG5TQpPdI-j4BjDqWGFIhkMwgDoir"
  }
  
  return(invisible(sim))
}


ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}


loader_env <- new.env(parent = globalenv())
sys.source(testthat::test_path("..", "..", "R", "downloadBCRModels.R"), loader_env)
model_fixture <- function() {
  bcr <- terra::vect(
    c("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
      "POLYGON ((2 0, 3 0, 3 1, 2 1, 2 0))"),
    crs = "EPSG:3857"
  )
  bcr$subUnit <- c(12, 13)
  files <- data.frame(
    name = c("RCKI_can13.Rdata", "GCKI_can12.Rdata", "GCKI_can13.Rdata",
             "RCKI_can12.Rdata", "AMRO_can12.Rdata", "AMRO_can13.Rdata",
             "GCKI_can14.Rdata", "README.txt"),
    id = paste0("remote-", seq_len(8))
  )
  list(studyArea = bcr, BCRdata = bcr, files = files)
}

mock_acquisition <- function(files) {
  state <- new.env(parent = emptyenv())
  state$downloads <- character(0)
  state$destination <- withr::local_tempdir(.local_envir = parent.frame())
  testthat::local_mocked_bindings(
    drive_ls = function(path, recursive) files,
    .package = "googledrive", .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    prepInputs = function(url, destinationPath, targetFile, fun, overwrite) {
      state$downloads <- c(state$downloads, url)
      path <- file.path(destinationPath, targetFile)
      saveRDS(list(mock = TRUE), path)
      path
    },
    .package = "reproducible", .env = parent.frame()
  )
  state
}

run_loader <- function(fixture, state, ...) {
  loader_env$downloadBCRModels(
    fixture$studyArea, fixture$BCRdata,
    "https://drive.google.com/drive/folders/fixture",
    state$destination, ...
  )
}

test_that("all downloaded models have unique species/BCR keys and existing paths", {
  fixture <- model_fixture()
  state <- mock_acquisition(fixture$files)
  out <- run_loader(fixture, state)
  expect_s3_class(out$modelIndex, "data.table")
  expect_identical(names(out$modelIndex),
                   c("species", "BCR", "fileName", "localPath", "remoteId"))
  expect_identical(data.table::key(out$modelIndex), c("species", "BCR"))
  expect_equal(nrow(out$modelIndex), 6L)
  expect_identical(out$modelFiles, out$modelIndex$localPath)
  expect_true(all(file.exists(out$modelIndex$localPath)))
  expect_identical(out$BCR_codes, c("12", "13"))
  expect_s4_class(out$BCR_parts, "SpatVector")
  expect_equal(nrow(out$BCR_parts), 2L)
  expect_equal(anyDuplicated(out$modelIndex, by = c("species", "BCR")), 0L)
  expect_setequal(state$downloads, out$modelIndex$remoteId)
  expect_identical(out$modelIndex$remoteId,
                   fixture$files$id[match(out$modelIndex$fileName, fixture$files$name)])
  expect_identical(basename(out$modelIndex$localPath), out$modelIndex$fileName)
  expect_identical(out$modelIndex$fileName,
                   paste0(out$modelIndex$species, "_can", out$modelIndex$BCR, ".Rdata"))
})

test_that("single and multiple species filters precede per-BCR limits", {
  fixture <- model_fixture()
  state <- mock_acquisition(fixture$files)
  single <- run_loader(fixture, state, species = " gcki ", testModelLimit = 1)
  expect_identical(single$modelIndex$species, rep("GCKI", 2))
  expect_setequal(single$modelIndex$BCR, c("12", "13"))
  multiple <- run_loader(fixture, state, species = c("GCKI", "RCKI", "GCKI"),
                         testModelLimit = 2)
  expect_equal(nrow(multiple$modelIndex), 4L)
  expect_setequal(multiple$modelIndex$species, c("GCKI", "RCKI"))
  limited <- run_loader(fixture, state, testModelLimit = 1)
  expect_identical(limited$modelIndex$species, rep("AMRO", 2))
  expect_error(run_loader(fixture, state, species = c("GCKI", "RCKI"),
                           testModelLimit = 1), "explicitly requested species")
})

test_that("missing requested models and uncovered BCRs fail before downloads", {
  fixture <- model_fixture()
  fixture$files <- fixture$files[fixture$files$name != "GCKI_can13.Rdata", ]
  state <- mock_acquisition(fixture$files)
  expect_error(run_loader(fixture, state, species = "GCKI"), "GCKI/13")
  expect_error(run_loader(fixture, state, species = "XXXX"), "XXXX/12.*XXXX/13")
  expect_length(state$downloads, 0L)
  fixture$files <- fixture$files[!grepl("can13", fixture$files$name), ]
  testthat::local_mocked_bindings(
    drive_ls = function(path, recursive) fixture$files, .package = "googledrive"
  )
  expect_error(run_loader(fixture, state), "No model files found for BCR\\(s\\): 13")
  expect_length(state$downloads, 0L)
})

test_that("unparseable filenames and duplicate keys fail before downloads", {
  fixture <- model_fixture()
  fixture$files$name[1] <- "bad_name_can13.Rdata"
  state <- mock_acquisition(fixture$files)
  expect_error(run_loader(fixture, state), "Unparseable model file names")
  fixture$files$name[1] <- "GCKI_can013.Rdata"
  testthat::local_mocked_bindings(
    drive_ls = function(path, recursive) fixture$files, .package = "googledrive"
  )
  expect_error(run_loader(fixture, state), "Duplicate species/BCR")
  expect_length(state$downloads, 0L)
})

test_that("BCR identifiers normalize and single-feature study areas select only their BCR", {
  fixture <- model_fixture()
  fixture$BCRdata$subUnit <- c("012", "013")
  fixture$studyArea <- fixture$studyArea[1, ]
  state <- mock_acquisition(fixture$files)
  out <- run_loader(fixture, state, species = "GCKI")
  expect_identical(out$BCR_codes, "12")
  expect_identical(out$modelIndex$BCR, "12")
  expect_identical(out$modelIndex$fileName, "GCKI_can12.Rdata")
})

test_that("invalid selection, limits and BCR inputs fail before downloads", {
  fixture <- model_fixture()
  state <- mock_acquisition(fixture$files)
  for (species in list(NA_character_, "", "GCKI.*", 1, c("GCKI", NA))) {
    expect_error(run_loader(fixture, state, species = species), "species must")
  }
  for (limit in list(0, -1, 1.5, Inf, NaN, 1+1i, TRUE, numeric(0), c(1, 2), "2")) {
    expect_error(run_loader(fixture, state, testModelLimit = limit), "testModelLimit")
  }
  fixture$BCRdata$subUnit <- c("12", NA)
  expect_error(run_loader(fixture, state), "subUnit")
  fixture$BCRdata$subUnit <- c("12", "13")
  fixture$studyArea <- terra::shift(fixture$studyArea, dx = 10)
  expect_error(run_loader(fixture, state), "No BCR units intersect")
  expect_length(state$downloads, 0L)
})

test_that("failed or ambiguous download paths cannot produce a model index", {
  fixture <- model_fixture()
  state <- mock_acquisition(fixture$files)
  testthat::local_mocked_bindings(
    prepInputs = function(...) file.path(state$destination, "missing.Rdata"),
    .package = "reproducible"
  )
  expect_error(run_loader(fixture, state, species = "GCKI"), "existing model file")
  testthat::local_mocked_bindings(
    prepInputs = function(...) state$destination, .package = "reproducible"
  )
  expect_error(run_loader(fixture, state), "existing model file")
  shared_path <- file.path(state$destination, "shared.Rdata")
  saveRDS(list(mock = TRUE), shared_path)
  testthat::local_mocked_bindings(
    prepInputs = function(...) shared_path, .package = "reproducible"
  )
  expect_error(run_loader(fixture, state, species = "GCKI"), "distinct local paths")
})

test_that("SpaDES reads the species parameter and every published output declaration", {
  metadata <- SpaDES.core::moduleMetadata(
    module = "bird_modelLoad", path = testthat::test_path("..", "..", "..")
  )
  expect_setequal(metadata$outputObjects$objectName,
                  c("modelIndex", "modelFiles", "BCR_codes", "BCR_parts"))
  classes <- metadata$outputObjects$objectClass
  names(classes) <- metadata$outputObjects$objectName
  expect_identical(classes[["modelIndex"]], "data.table")
  expect_identical(classes[["BCR_codes"]], "character")
  species_param <- metadata$parameters[metadata$parameters$paramName == "species", ]
  expect_equal(nrow(species_param), 1L)
  expect_identical(species_param$paramClass[[1L]], "character")
})

test_that("SpaDES loads the helper and Init publishes the selected model index", {
  fixture <- model_fixture()
  state <- mock_acquisition(fixture$files)
  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))
  testthat::local_mocked_bindings(
    Require = function(...) {
      for (pkg in c("data.table", "ggplot2", "googledrive", "reproducible", "sf", "terra")) {
        library(pkg, character.only = TRUE)
      }
      invisible(TRUE)
    },
    .package = "Require"
  )
  sim <- SpaDES.core::simInit(
    times = list(start = 0, end = 1),
    modules = "bird_modelLoad",
    params = list(bird_modelLoad = list(species = c("GCKI", "RCKI"))),
    objects = list(studyArea = fixture$studyArea, BCRdata = fixture$BCRdata,
                   modelfolderURL = "https://drive.google.com/drive/folders/fixture"),
    paths = list(modulePath = testthat::test_path("..", "..", ".."),
                 inputPath = state$destination,
                 outputPath = file.path(state$destination, "outputs"),
                 cachePath = file.path(state$destination, "cache"))
  )
  expect_s4_class(sim, "simList")
  expect_true(is.function(sim$.mods$bird_modelLoad$downloadBCRModels))
  expect_identical(SpaDES.core::params(sim)$bird_modelLoad$species, c("GCKI", "RCKI"))
  expect_length(state$downloads, 0L)
  init_event <- SpaDES.core::events(sim)
  init_event <- init_event[init_event$moduleName == "bird_modelLoad" &
                            init_event$eventType == "init", ]
  expect_equal(nrow(init_event), 1L)
  SpaDES.core::current(sim) <- init_event
  out <- sim$.mods$bird_modelLoad$Init(sim)
  expect_s4_class(out, "simList")
  expect_equal(nrow(out$modelIndex), 4L)
  expect_setequal(out$modelIndex$species, c("GCKI", "RCKI"))
  expect_identical(out$modelFiles, out$modelIndex$localPath)
  expect_identical(out$BCR_codes, c("12", "13"))
  expect_s4_class(out$BCR_parts, "SpatVector")
})

#' Download models and index them by species and BCR
#'
#' @param studyArea Study-area polygon SpatVector.
#' @param BCRdata BCR polygon SpatVector with numeric or digit-only subUnit codes.
#' @param modelFolderURL Google Drive folder URL.
#' @param destinationPath Directory receiving downloaded files.
#' @param testModelLimit Positive whole-number limit per BCR, or NA for no limit.
#' @param species Species codes; character(0) selects all available species.
#'
#' @return List containing modelIndex (a keyed data.table with species, BCR,
#'   fileName, localPath and remoteId), modelFiles, BCR_codes and BCR_parts.
#' @keywords internal
downloadBCRModels <- function(studyArea, BCRdata, modelFolderURL, destinationPath,
                              testModelLimit = NA_real_, species = character(0)) {
  if (!is.character(species) || anyNA(species) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9]*$", trimws(species)))) {
    stop("species must be a character vector of species codes, or character(0).")
  }
  requested_species <- unique(toupper(trimws(species)))
  if (!(is.integer(testModelLimit) || is.double(testModelLimit) ||
        identical(testModelLimit, NA)) || length(testModelLimit) != 1L ||
      is.nan(testModelLimit) ||
      (!is.na(testModelLimit) &&
       (!is.finite(testModelLimit) || testModelLimit < 1 ||
        testModelLimit != floor(testModelLimit)))) {
    stop("testModelLimit must be NA or a positive whole-number scalar.")
  }
  if (!is.na(testModelLimit) && testModelLimit < length(requested_species)) {
    stop("testModelLimit must be at least the number of explicitly requested species.")
  }
  if (!inherits(studyArea, "SpatVector") || !inherits(BCRdata, "SpatVector") ||
      nrow(studyArea) == 0L || nrow(BCRdata) == 0L ||
      terra::geomtype(studyArea) != "polygons" ||
      terra::geomtype(BCRdata) != "polygons") {
    stop("studyArea and BCRdata must be nonempty polygon SpatVectors.")
  }
  if (!all(terra::is.valid(studyArea)) || !all(terra::is.valid(BCRdata))) {
    stop("studyArea and BCRdata must have valid geometries.")
  }
  if (!nzchar(terra::crs(studyArea)) || !nzchar(terra::crs(BCRdata))) {
    stop("studyArea and BCRdata must have defined coordinate reference systems.")
  }
  if (!terra::same.crs(BCRdata, studyArea)) {
    BCRdata <- terra::project(BCRdata, terra::crs(studyArea))
  }
  codes <- as.character(BCRdata$subUnit)
  if (length(codes) != nrow(BCRdata) || anyNA(codes) ||
      any(!grepl("^[0-9]+$", codes))) {
    stop("BCRdata must contain nonmissing digit-only subUnit identifiers.")
  }
  codes <- sub("^0+(?=[0-9])", "", codes, perl = TRUE)
  # relate returns one column per study-area feature, not a row-index vector.
  idx <- which(rowSums(terra::relate(BCRdata, studyArea, "intersects")) > 0L)
  if (!length(idx)) {
    stop("No BCR units intersect studyArea.")
  }
  BCR_parts <- terra::intersect(BCRdata[idx, ], studyArea)
  if (nrow(BCR_parts) == 0L) {
    stop("BCR intersections contain no polygons within studyArea.")
  }
  bcr_codes <- unique(codes[idx])
  message("Intersecting BCR(s): ", paste(bcr_codes, collapse = ", "))

  files <- googledrive::drive_ls(googledrive::as_id(modelFolderURL), recursive = TRUE)
  pattern <- "^([A-Za-z][A-Za-z0-9]*)_can([0-9]+)\\.Rdata$"
  model_rows <- which(grepl("\\.Rdata$", files$name))
  invalid <- files$name[model_rows][!grepl(pattern, files$name[model_rows])]
  if (length(invalid)) {
    stop("Unparseable model file names; expected SPECIES_canBCR.Rdata: ",
         paste(invalid, collapse = ", "))
  }
  file_names <- files$name[model_rows]
  modelIndex <- data.table::data.table(
    species = toupper(sub(pattern, "\\1", file_names)),
    BCR = sub("^0+(?=[0-9])", "", sub(pattern, "\\2", file_names), perl = TRUE),
    fileName = file_names,
    localPath = rep(NA_character_, length(model_rows)),
    remoteId = as.character(files$id[model_rows])
  )
  modelIndex <- modelIndex[modelIndex$BCR %in% bcr_codes, ]
  if (length(requested_species)) {
    modelIndex <- modelIndex[modelIndex$species %in% requested_species, ]
    required <- expand.grid(species = requested_species, BCR = bcr_codes,
                            stringsAsFactors = FALSE)
    missing <- !paste(required$species, required$BCR) %in%
      paste(modelIndex$species, modelIndex$BCR)
    if (any(missing)) {
      stop("Missing requested species/BCR models: ",
           paste(paste(required$species[missing], required$BCR[missing], sep = "/"),
                 collapse = ", "))
    }
  }
  missing_bcr <- setdiff(bcr_codes, modelIndex$BCR)
  if (length(missing_bcr)) {
    stop("No model files found for BCR(s): ", paste(missing_bcr, collapse = ", "))
  }
  if (anyDuplicated(modelIndex, by = c("species", "BCR"))) {
    stop("Duplicate species/BCR model keys in the selected Google Drive files.")
  }
  if (anyNA(modelIndex$remoteId) || any(!nzchar(modelIndex$remoteId))) {
    stop("Selected model files must have nonempty Google Drive IDs.")
  }
  data.table::setorder(modelIndex, fileName)
  if (!is.na(testModelLimit)) {
    message("Test mode active: limiting to ", testModelLimit, " models per BCR")
    keep <- ave(seq_len(nrow(modelIndex)), modelIndex$BCR, FUN = seq_along) <=
      testModelLimit
    modelIndex <- modelIndex[keep, ]
  }

  for (i in seq_len(nrow(modelIndex))) {
    message(" Downloading model for: ", modelIndex$fileName[i])
    local_path <- reproducible::prepInputs(
      url = modelIndex$remoteId[i],
      destinationPath = destinationPath,
      targetFile = modelIndex$fileName[i],
      fun = NA,
      overwrite = TRUE
    )
    if (!is.character(local_path) || length(local_path) != 1L ||
        is.na(local_path) || !file.exists(local_path) || dir.exists(local_path)) {
      stop("Download did not return an existing model file for: ", modelIndex$fileName[i])
    }
    data.table::set(modelIndex, i, "localPath", normalizePath(local_path, mustWork = TRUE))
  }
  if (anyDuplicated(modelIndex$localPath)) {
    stop("Downloaded models must have distinct local paths.")
  }
  data.table::setkeyv(modelIndex, c("species", "BCR"))
  message("Downloaded ", nrow(modelIndex), " model(s) to: ", destinationPath)
  list(
    modelIndex = modelIndex,
    modelFiles = modelIndex$localPath,
    BCR_codes = bcr_codes,
    BCR_parts = BCR_parts
  )
}

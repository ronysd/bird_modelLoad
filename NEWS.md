# bird_modelLoad 0.0.6 (3 November 2026)

- Explicitly visit species folders and nested subfolders when listing remote
  models, rather than relying on a combined recursive listing. Folder IDs are
  visited once, duplicate remote IDs are deduplicated, and listing errors stop
  acquisition. Added nested GCKI/RCKI coverage tests for BCRs 12, 71, 80 and 81
  (ML-H2, ML-H4, ML-M4).

# bird_modelLoad 0.0.5 (3 November 2026)

- Restrict filename validation to Canadian model candidates, preserving the
  original Canadian-only selection. US models and unrelated Rdata files in
  mixed-country folders no longer cause errors or satisfy Canadian coverage.
  Added mixed-country regression tests (ML-H2, ML-H4, ML-M4).

# bird_modelLoad 0.0.2 (3 November 2026)

- Added `modelIndex`, listing downloaded models by species and BCR with file paths
  and Google Drive IDs; declared all existing outputs (ML-H1, ML-H2).
- Added the optional `species` filter, e.g. `c("GCKI", "RCKI")`. Missing requested
  species/BCR pairs stop acquisition before downloading.
- Validate model names, duplicate keys, BCR coverage, test limits, and downloaded
  paths; handle study areas with multiple polygon features (ML-H4, ML-M4).
- Normalize BCR identifiers to character codes without leading zeros.
- Declare installed acquisition/index dependencies and remove reliance on an
  attached dplyr package for model selection.

# bird_modelLoad 0.0.1 (11 June 2025)

- initial module version
# bird_modelLoad 0.0.3 (3 November 2026)

- Added opt-in `repairInvalidBCR = TRUE` to repair invalid BCR features locally,
  with warnings and checks for valid nonempty polygons and preserved feature
  identity. The caller's BCRdata and studyArea are not modified (ML-H4).
- Geometry errors now identify the input, feature numbers, and validity reasons;
  BCR validity is checked after reprojection. Removed a debugging pause.
- Added mocked regression coverage for geometry repair and its parameter (ML-M4).
# bird_modelLoad 0.0.4 (3 November 2026)

- BCR geometry repair is now automatic; removed the `repairInvalidBCR` parameter.
  Remove this parameter from existing setup calls. Repair still warns, validates
  the result, and preserves caller-owned inputs (ML-H4, ML-M4).

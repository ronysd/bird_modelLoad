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

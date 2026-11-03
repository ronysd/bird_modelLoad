# bird_modelLoad

Download bird models for BCR units intersecting a study area and list them in
`sim$modelIndex`, a data.table keyed by `species` and `BCR`. Each row includes
`fileName`, `localPath`, and the Google Drive `remoteId`.

```r
parameters <- list(bird_modelLoad = list(species = c("GCKI", "RCKI")))
# species = "GCKI" selects one species; character(0) (default) selects all.
sim$modelIndex[species == "GCKI" & BCR == "12"]
```

Every requested species must be available in every intersecting BCR; missing
pairs stop acquisition before downloading. `testModelLimit` applies after species
filtering and cannot exclude explicitly requested species.

Models use filenames such as `GCKI_can12.Rdata`. Downloads retain the existing
overwrite policy and use `inputPath(sim)`. `modelFiles`, `BCR_codes` (now character),
and `BCR_parts` remain available. No fitted models are loaded into memory.

The predictor has not been rewired to use the index; previously downloaded files
in a shared model folder are not removed by a species-filtered run.

See the [module manual](bird_modelLoad.Rmd) for inputs and the complete output
contract. Run mocked acquisition tests from the module directory with
`testthat::test_dir("tests/testthat")`; no live model downloads are performed.

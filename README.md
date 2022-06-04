# How to run

```r
# move to the scripts directory
setwd("<path to project>/econ-project/scripts/")
# source the main script, as it will call the subsections
# in the correct order
source("main.R")
```
## Notes

- All required packages are loaded in `main.R`.
- The timeframe of interest can be specified in `main.R` (by default it considers all available data).
- The `logs` directory contains pre-run outputs for:
  - the entire timeframe,
  - the period before the structural break,
  - the period after the structural break.

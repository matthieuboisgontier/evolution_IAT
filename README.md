# Evolution of attitudes toward people with disabilities in healthcare practitioners and other occupations from 2006 to 2024: material, data, and R script

## Overview
This repository contains data and an `R` script to examine the evolution of explicit and implicit attitudes toward physical disability in clinicians, rehabilitation assistants, and individuals in other occupations using the [Physical Disability IAT dataset](https://doi.org/10.17605/OSF.IO/Y9HIQ), collected from 2006 to 2024 via the [Project Implicit](https://implicit.harvard.edu/implicit/selectatest.html) and made available on the Open Science Framework (OSF) under the CC0 1.0 Universal license.

## Data
The R script expect the SAV filel that are subsequently imported by `R`.

## Files
Disability_IAT.public.2004 to Disability_IAT.public.2024

## Prerequisites
### R version
4.4.1 or higher

### R Packages
The script assume/require the following packages to be installed in `R`:
- [`haven`](https://github.com/hadley/haven)
- [`here`](https://github.com/jennybc/here)
- [`dplyr`](https://github.com/tidyverse/dplyr)
- [`tidyr`](https://github.com/tidyverse/tidyr)
- [`stringr`](https://github.com/tidyverse/stringr)
- [`effects`](https://github.com/cran/effects)
- [`lubridate`](https://github.com/tidyverse/lubridate)
- [`mgcv`](https://github.com/cran/mgcv)
- [`ggplot2`](https://github.com/tidyverse/ggplot2)
- [`broom`](https://github.com/tidymodels/broom)
- [`viridis`](https://github.com/sjmgarnier/viridis)
- [`patchwork`](https://github.com/thomasp85/patchwork)

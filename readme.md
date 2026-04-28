# coherenceD
Marcello D’Orazio

# coherenceD

> **R code and tools for assessing <u>coherence</u> between estimated
> distributions**

------------------------------------------------------------------------

## Overview

**coherenceD** collects R functions developed to handle data observed on
the same variable across two distinct data sources, specifically:

-   two **probability sample surveys** with complex designs and unequal
    unit weights
-   a **probability survey** and a **non-probability data source**

The main goal is to assess the **coherence between distributions**
estimated from these different sources, addressing the challenges that
arise from differences in sampling designs, weighting schemes, and data
collection methods.

> ⚠️ This is a work-in-progress repository. The functions collected here
> are expected to be incorporated into a standalone R package in the
> future.

------------------------------------------------------------------------

## Repository Contents

-   **`R/`** — R functions for coherence assessment between estimated
    distributions
-   **`presentation uRos 2023/`** — Slides and material presented at the
    [uRos 2023 conference](http://r-project.ro/conference2023.html)

------------------------------------------------------------------------

## Installation

Since the package is not yet on CRAN, you can source the functions
directly from this repository:

``` r
# install.packages("devtools")
devtools::install_github("marcellodo/coherenceD")
```

------------------------------------------------------------------------

## Related packages

This work complements the following R packages by the same author:

-   [**StatMatch**](https://github.com/marcellodo/StatMatch) —
    statistical matching and data integration methods
-   [**univOutl**](https://github.com/marcellodo/univOutl) — univariate
    and bivariate outlier detection

------------------------------------------------------------------------

## Author

**Marcello D’Orazio**  
Research Manager, Statistical Methods  
Italian National Institute of Statistics (Istat)  
🔗 [ResearchGate](https://www.researchgate.net/profile/Marcello-Dorazio)
\| [LinkedIn](https://www.linkedin.com/in/marcello-d-orazio-2802651)

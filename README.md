# FAO-OECD Employment Data Imputation Tool (FO-EDIT)

## Getting started

### Prerequisites

To run the tool, you need the following installed on your computer.
- A recent R installation (I used version 4.1.0 to build the tool)
- A recent RStudio installation (I use 1.4.1103)
- The following R packages installed: dplyr, ggplot2, readr, tidyr, tibble, compareDF, stargazer, gridExtra, Rilostat, OECD. 

You can install them by running the following code in R:  
```
install.packages(
  c("dplyr", "ggplot2", "readr", "tidyr", "tibble", "compareDF", "stargazer", "gridExtra", "Rilostat", "OECD")
  )
```

### Installation

1. Extract the contents of the compressed folder to the destination of your choice (e.g. on your Desktop).
2. Double-click the "emputator.Rproj" file in the main folder.
3. The tool will open in RStudio.

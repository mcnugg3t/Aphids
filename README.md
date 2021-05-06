# Aphids
Modeling Midwestern Aphid Population Dynamics with Varimax and Poisson Regression

## Abstract
For our project, we chose to examine aphid abundance across the Midwest as it corresponds with  environmental factors as well as crop presence. We combined data from the soybean aphid suction trap network, crop data from the USDA’s National Agricultural Statistics Service CropScape project, and weather data from NOAA’s Physical Sciences Laboratory’s NCEP-DOE Reanalysis 2. We endeavored to use unsupervised learning, namely Varimax rotation of Principal Component Analysis (PCA), to improve accounting of the environmental factors responsible for aphid population dynamics. We predicted that, after controlling for weather factors, crop monocultures would be associated with lower aphid diversity, with implications for sustainable, effective, and minimally destructive agriculture. 

#### Clone the repo.
```
$ git clone https://github.com/mcnugg3t/Aphids
```

#### How to run the code in terminal?
```
R -e "shiny::runApp('/path/to/Aphids/')"
```

#### How to run the code in R?
``` r
install.packages("shiny")
library(shiny)
runApp("path/to/Aphids")
```

#### OR

```r
install.packages("shiny")
library(shiny)
runGitHub("Aphids","mcnugg3t",ref="main")
```

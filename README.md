
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BIONICv3

<!-- badges: start -->

<!-- badges: end -->

BIONICv3 is an R package implementation of the “BIONIC” model by Armitage et al. (2013) which is a toxicokinetic model designed to predict bioconcentration factors (internal / external concentration ratios) for a wide range of chemical groups including ionisable compounds. The version of the model here is based explicitly on the VBA tool and BIONIC v3.0 User Manual and Technical Guidance Document (https://sites.google.com/view/bionic-v3/home) The package uses graphs to dynamically propagate default and calculated values throughout the model, allowing for complete flexibility for inputs and future model adaption.

## Installation

**This will change for published version**

To install and use the package locally, first clone the repository using (assumes access to DevOps):

    git clone https://github.com/seacunilever/R-BIONIC.git

Then install the package using the following commands (note, this requires devtools) 

``` r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install(build_vignettes = TRUE)

# later likely replaced by devtools::install_github("INSERT PATH HERE")
```

It is recommended that the user installs the package with `build_vignettes = TRUE` as these contain helpful guidance and important information about the package and model.
<br>
You can then verify that the installation was successful by running

``` r
library(BIONICv3)
```

## Example

For the generic use of the BIONICv3 default model, use the `run_bionic_v3()` function. A typical input for this function can be produced as such:

``` r
example <-as.data.frame( list("biota_mass" = 0.01, 
                               "ion_type"="A", 
                               "pka"=3.9, 
                               "KOWN"=10^5.5, 
                               "reaction_rate"=0.5,
                               "water_temp"=12))
knitr::kable(example)
```

| biota_mass | ion_type | pka |     KOWN | reaction_rate | water_temp |
|-----------:|:---------|----:|---------:|--------------:|-----------:|
|       0.01 | A        | 3.9 | 316227.8 |           0.5 |         12 |

This example shows the minimum number of inputs for the model to be able to generate a prediction. However, it's possible to provide more complex data frames with additional columns of chosen values and the model will still be able to process this.

Here is then an example of how to run the model to generate BCF predictions. Note that the output is a list containing all parameters as they are calculated through the model in a list format. These can be converted to a data frame for further processing, e.g. keeping only the output values of interest.

``` r
answer_list <- run_bionic_v3(example)
answer_df <- as.data.frame(answer_list)
bcf <- answer_df[,c("gill_uptake_rate",
                     "gill_elimination_rate",
                     "biotransformation_rate",
                     "fecal_egestion_rate",
                     "growth_rate",
                     "bcf")]
knitr::kable(bcf)
```

| gill_uptake_rate | gill_elimination_rate | biotransformation_rate | fecal_egestion_rate | growth_rate | bcf |
|---:|---:|---:|---:|---:|---:|
| 10.95765 | 0.0113926 | 0.2857498 | 0.0006206 | 0.0015701 | 37.40069 |

## BIONICv3 default graph

Here is the graph representation of the full BIONIC model, showing the scale and connectedness of the model's parameter space.

![](./man/figures/full_graph.png)

As it is difficult to view the large graph, we recommend viewing the "quick-start-guide" vignette as this has an interactive HTML render of the same graph. To view, run the following command and select [HTML].

``` r
utils::browseVignettes(package="BIONICv3")
```

</div>

</div>

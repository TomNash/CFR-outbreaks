# Vaccine-Preventable Outbreaks

I've taken data from the [CFR on vaccine-preventable outbreaks](http://www.cfr.org/interactives/GH_Vaccine_Map/#map) to create maps showing the likelihood of other outbreaks using [maximum entropy modeling](http://homepages.inf.ed.ac.uk/lzhang10/maxent.html). Currently the range of predictors is climatic but I hope to add socioeconomic ones as well such as population density.

They Shiny app can be run with the command `runGitHub()` using the `shiny` package, everything should be handled automatically from there with regards to downloading and unpacking data. You'll need the `shiny` and `shinyBS` packages installed along with `dismo` before running. The model does take a while to run (it's a Java backend and it depends on number of predictors chosen).

*Note:* Installing/running `dismo::maxent` can be an cumbersome, see the [documentation](https://cran.r-project.org/web/packages/dismo/dismo.pdf#maxent) for more guidance. You will need to provide information at the provider's site in order to download the `.jar` file which will be then placed in the `system.file("java", package="dismo")` directory.

```
library(shiny)
library(shinyBS)
runGitHub("TomNash/CFR-outbreaks", launch.browser = TRUE)
```


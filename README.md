# cjbae

`cjbae` provides functions for Bayesian analysis of conjoint experimental data. These functions use [`brms`](https://github.com/paul-buerkner/brms) and [`emmeans`](https://github.com/rvlenth/emmeans) to estimate Bayesian average marginal component effects (AMCEs) and marginal means (MMs). For explanations of these quantities of interest in the standard frequentist approach to conjoint analysis, see [Hainmueller, Hopkins and Yamamoto (2014)](https://www.cambridge.org/core/journals/political-analysis/article/causal-inference-in-conjoint-analysis-understanding-multidimensional-choices-via-stated-preference-experiments/414DA03BAA2ACE060FFE005F53EFF8C8) and [Leeper, Hobolt and Tilley (2018)](https://s3.us-east-2.amazonaws.com/tjl-sharing/assets/MeasuringSubgroupPreferences.pdf).    

[`amce_bae()`](https://github.com/mbarnfield/cjbae/blob/master/R/amce_bae.R) estimates distributions of AMCE parameters for all levels (excluding baselines/reference categories) of the specified (in a formula) features in a given tidy conjoint dataset (see `cregg`'s [`cj_tidy`](https://github.com/leeper/cregg/blob/master/R/cj_tidy.R)). It does this using [`brms`](https://github.com/paul-buerkner/brms). As such, `cjbae` depends on `brms`, and unfortunately installation of the latter can be a bit tricky if you don't have [`rstan`](https://mc-stan.org/users/interfaces/rstan) and a C++ compiler functioning on your machine. [This page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) should make your life easier if so.     

[`mm_bae()`](https://github.com/mbarnfield/cjbae/blob/master/R/mm_bae.R) takes an `amce_bae` object of class `brmsfit` and calculates marginal means for all feature-levels, as specified in a formula. Note that this function therefore requires a `brmsfit` output, so you'll need to have run `amce_bae` first, with `save_amce = TRUE`. It is this *saved* object that will need to be passed to `mm_bae`. The main output of `amce_bae` is a tibble, which will not work with `mm_bae`. The saved object can be loaded into your environment using `readRDS()`.    

Alternatively, [`cjbae()`](https://github.com/mbarnfield/cjbae/blob/master/R/cjbae.R) is a one-stop shop for both AMCE and MM calculation. Specifying `estimate = "mm"` and `save_amce = TRUE` will generate a saved AMCE `brmsfit` object and a tidy dataframe of MMs in your R environment. The function [`cjbae_df()`](https://github.com/mbarnfield/cjbae/blob/master/R/cjbae_df.R) can then be used on the `brmsfit` object, once it is loaded into your environment, to create a tidy dataframe of AMCEs. 

Both MMs and AMCEs can be passed to [`cjbae_plot()`](https://github.com/mbarnfield/cjbae/blob/master/R/cjbae_plot.R) to plot the distributions of parameter estimates in a way that is more-or-less visually consistent with [standard approaches to plotting conjoint estimates](https://github.com/leeper/cregg/blob/master/R/plot_cj_amce.R). This function is currently not particularly flexible, and can be easily replicated in a more flexible way with a standard `ggplot` approach.     

The name `cjbae` is obviously a sort of 'conjoint bayesian' portmanteau, but it conveniently also reminds us that **'conjoint analysis is bae'**.

To install this developmental version of the package:

```R
if (!require("remotes")) {
    install.packages("remotes")
}
remotes::install_github("mbarnfield/cjbae")
```

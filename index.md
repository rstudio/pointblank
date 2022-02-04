# pointblank <img src="man/figures/logo.svg" align="right" alt="" width="120" />

<!-- badges: start -->
<a href="https://cran.r-project.org/package=pointblank"><img src="https://www.r-pkg.org/badges/version/pointblank" alt="CRAN status" /></a>
<a href="https://github.com/rich-iannone/pointblank/actions"><img src="https://github.com/rich-iannone/pointblank/workflows/R-CMD-check/badge.svg" alt="R build status" /></a>
<a href="https://github.com/rich-iannone/pointblank/actions"><img src="https://github.com/rich-iannone/pointblank/workflows/lint/badge.svg" alt="Linting" /></a>
<a href="https://codecov.io/gh/rich-iannone/pointblank?branch=master"><img src="https://codecov.io/gh/rich-iannone/pointblank/branch/master/graph/badge.svg" alt="Coverage status" /></a>
<a href="https://bestpractices.coreinfrastructure.org/projects/4310"><img src="https://bestpractices.coreinfrastructure.org/projects/4310/badge" alt="Best Practices"></a>
<a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="The project has reached a stable, usable state and is being actively developed." /></a>
<a href="https://CRAN.R-project.org/package=pointblank"><img src="https://cranlogs.r-pkg.org/badges/pointblank" alt="Monthly Downloads"></a>
<a href="https://CRAN.R-project.org/package=pointblank"><img src="https://cranlogs.r-pkg.org/badges/grand-total/pointblank" alt="Total Downloads"></a>
[![RStudio community](https://img.shields.io/badge/RStudio%20Cloud-pointblank%20Test%20Drive-blue?style=social&logo=rstudio&logoColor=75AADB)](https://rstudio.cloud/project/3411822)
<a href="https://www.contributor-covenant.org/version/2/0/code_of_conduct/"><img src="https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg" alt="Contributor Covenant" /></a>
<!-- badges: end -->

<br />
With the **pointblank** package it’s really easy to methodically validate your
data whether in the form of data frames or as database tables. On top of the
validation toolset, the package gives you the means to provide and keep
up-to-date with the information that *defines* your tables.

For table *validation*, the *agent* object works with a large collection of
simple (yet powerful!) validation functions. We can enable much more
sophisticated validation checks by using custom expressions and also through
stepwise mutation of the target table (through something we call
`preconditions`).

Sometimes we want to maintain table *information* and update it when the table
goes through changes. For that, we can use an *informant* object + associated
functions to help define the metadata entries and present it in a way that suits
you.

<hr>
<div style="text-align: center;">
<section id="video">
<div class="videoUiWrapper thumbnail">
<video width="70%" controls="" id="video" poster="img/video.png">
<source src="https://silly-jackson-b3dec8.netlify.app/pointblank_validation.mp4" type="video/mp4">
</video>
</div>
</div>
</section>
<hr>

## Installation

Want to try this out? The **pointblank** package is available on
**CRAN**:

``` r
install.packages("pointblank")
```

You can also install the development version of **pointblank** from
**GitHub**:

``` r
devtools::install_github("rich-iannone/pointblank")
```

If you encounter a bug, have usage questions, or want to share ideas to
make this package better, feel free to file an
[issue](https://github.com/rich-iannone/pointblank/issues).

## Let's Discuss!

Let's talk about data validation and data documentation in
[**pointblank** Discussions](https://github.com/rich-iannone/pointblank/discussions)! It's a great place to ask questions about how to use the package, discuss
some ideas, engage with others, and much more!

## Code of Conduct

Please note that the pointblank project is released with a [contributor
code of conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/). By participating in this project you agree to abide by its terms.

## 📄 License

**pointblank** is licensed under the MIT license.
See the [`LICENSE.md`](LICENSE.md) file for more details.

## 🏛️ Governance

This project is primarily maintained by [Rich Iannone](https://www.twitter.com/riannone). Other authors may occasionally assist with some of these duties.

# pointblank <img src="man/figures/logo.svg" align="right" alt="" width="120" />

<!-- badges: start -->
<a href="https://cran.r-project.org/package=pointblank"><img src="https://www.r-pkg.org/badges/version/pointblank" alt="CRAN status" /></a>
<a href="https://github.com/rich-iannone/pointblank/actions"><img src="https://github.com/rich-iannone/pointblank/workflows/R-CMD-check/badge.svg" alt="R build status" /></a>
<a href="https://github.com/rich-iannone/pointblank/actions"><img src="https://github.com/rich-iannone/pointblank/workflows/pkgdown/badge.svg" alt="Package Site" /></a>
<a href="https://github.com/rich-iannone/pointblank/actions"><img src="https://github.com/rich-iannone/pointblank/workflows/lint/badge.svg" alt="Linting" /></a>
<a href="https://codecov.io/gh/rich-iannone/pointblank?branch=master"><img src="https://codecov.io/gh/rich-iannone/pointblank/branch/master/graph/badge.svg" alt="Coverage status" /></a>
<a href="https://bestpractices.coreinfrastructure.org/projects/4310"><img src="https://bestpractices.coreinfrastructure.org/projects/4310/badge" alt="Best Practices"></a>
<a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="The project has reached a stable, usable state and is being actively developed." /></a>
<a href="https://CRAN.R-project.org/package=pointblank"><img src="https://cranlogs.r-pkg.org/badges/pointblank" alt="Monthly Downloads"></a>
<a href="https://CRAN.R-project.org/package=pointblank"><img src="https://cranlogs.r-pkg.org/badges/grand-total/pointblank" alt="Total Downloads"></a>
<a href="https://www.contributor-covenant.org/version/2/0/code_of_conduct/"><img src="https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg" alt="Contributor Covenant" /></a>
<!-- badges: end -->


Con el paquete **pointblank** es realmente fácil validar metódicamente sus datos, ya sea en forma de marcos de datos o de tablas de bases de datos. Además del conjunto de herramientas de herramientas de validación, el paquete le ofrece los medios para proporcionar y mantener la información que *define* sus tablas.

Para la *validación* de tablas, el objeto *agente* trabaja con una gran colección de funciones de validación sencillas (¡pero potentes!). Podemos habilitar comprobaciones de validación mucho más sofisticadas utilizando expresiones personalizadas y también mediante la mutación gradual de la tabla de destino (a través de algo que llamamos `preconditions`).

A veces queremos mantener la *información* de la tabla y actualizarla cuando la tabla sufra cambios. Para ello, podemos utilizar un objeto *informant* + funciones asociadas funciones para ayudar a definir las entradas de metadatos y presentarlas de una manera que le convenga.

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

¿Quieres probarlo? El paquete **pointblank** está disponible en **CRAN**:

``` r
install.packages("pointblank")
```

También puedes instalar la versión de desarrollo de **pointblank** desde **GitHub**:

``` r
devtools::install_github("rich-iannone/pointblank")
```

Si encuentra un error, tiene preguntas de uso, o quiere compartir ideas para mejorar este paquete, no dudes en presentar un [issue](https://github.com/rich-iannone/pointblank/issues).

## ¡Discutamos!

Hablemos de la validación de datos y de la documentación de datos en
[**pointblank** Discussions](https://github.com/rich-iannone/pointblank/discussions). Es un gran lugar para hacer preguntas sobre cómo usar el paquete, discutir algunas ideas, participar con otros y mucho más.

## Código de conducta

Tenga en cuenta que el proyecto pointblank se publica con un [código de conducta para los colaboradores](https://www.contributor-covenant.org/version/2/0/code_of_conduct/). Al participar en este proyecto, usted se compromete a respetar sus condiciones.

## 📄 Licencia

**pointblank** está licenciado bajo la licencia MIT.
Consulte el archivo [`LICENSE.md`](LICENSE.md) para obtener más detalles.

## 🏛️ Gobernanza

 Este proyecto es mantenido principalmente por [Rich Iannone](https://www.twitter.com/riannone). Otros autores pueden ayudar ocasionalmente en algunas de estas tareas.

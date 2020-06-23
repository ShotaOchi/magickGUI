# magickGUI

[![Build Status](https://travis-ci.org/ShotaOchi/magickGUI.svg?branch=master)](https://travis-ci.org/ShotaOchi/magickGUI)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ShotaOchi/magickGUI?branch=master&svg=true)](https://ci.appveyor.com/project/ShotaOchi/magickGUI)
[![CRAN Version](https://www.r-pkg.org/badges/version/magickGUI)](https://cran.r-project.org/package=magickGUI)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![codecov](https://codecov.io/gh/ShotaOchi/magickGUI/branch/master/graph/badge.svg)](https://codecov.io/gh/ShotaOchi/magickGUI)

## Note

magickGUI doesn't work on R 4.0.1 for Windows although magickGUI works fine on R 4.0.2 for Windows.

It's the CRAN's fault.

I can do nothing about it.

Don't use R 4.0.1 for Windows if you use magickGUI package.

## Purpose
The purpose of magickGUI is to enable us to use the functions of the package 'magick' interactively.

## Installation
you can install magickGUI from CRAN or GitHub.

Run the following R code to install magickGUI.
```r
# install from CRAN
install.packages("magickGUI")
# install from Github
devtools::install_github("ShotaOchi/magickGUI")
```

## Simple Demo
You can use image_threshold function of 'magick' interactively.

Run the following code.
```r
library(magickGUI)
interactive_threshold(wizard)
```

## Naming Rule
Just replace "image" with "interactive".

For example, image_threshold &rarr; interactive_threshold.

## Contribution
You're welcome to create issues for any bug report or suggestion on the [issues page](https://github.com/ShotaOchi/magickGUI/issues).

You can also fork this repository and send me a pull request for bug fixes or additional features.
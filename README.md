
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AIPAL: Artificial Intelligence-based Prediction of Acute Leukemia
https://www.thelancet.com/journals/landig/article/PIIS2589-7500(24)00044-X/fulltext


**Evaluation of a machine-learning model based on laboratory parameters 
for the prediction of acute leukaemia subtypes: a multicentre model development 
and validation study in France**

Lancet Digit Health. 2024 May;6(5):e323-e333. PMID: 38670741 DOI: 10.1016/S2589-7500(24)00044-X 

*Vincent Alcazer, Grégoire Le Meur, Marie Roccon, Sabrina Barriere,
Baptiste Le Calvez, Bouchra Badaoui, Agathe Spaeth, Olivier Kosmider,
Nicolas Freynet, Marion Eveillard5, Carolyne Croizier, Simon Chevalier,
Pierre Sujobert*

**Methods**

We conducted a multicentre, model development, and validation study
based on 19 routine laboratory parameters collected at disease onset in
1410 acute leukaemia patients from six independent French University
Hospitals. Using training (n=679) and external validation (n=731)
cohorts, several machine learning models were evaluated with a custom
sensitivity analysis for variable selection. An additional prospective
cohort of n=66 patients was also used for further validation.

Based on ten routine laboratory parameters, our final eXtreme Gradient
Boosting (XGB)-model showed an AUC \[95%CI\] of 0.97 \[0.95-0.99\], 0.90
\[0.83-0.97\], and 0.89 \[0.82-0.95\] for APL, ALL, and AML diagnoses,
respectively. Optimal cutoffs to guide clinical decisions were then set,
leading to an accuracy of 99.7/99.5/98.8% for confident predictions and
96.1/87.9/86.3% for overall predictions of APL, ALL, and AML,
respectively. These results were confirmed in the prospective cohort.
The final model was integrated into a web-app with a user-friendly
graphical interface, AI-PAL.

# Getting started

AI-PAL is a free and open-source software package built in R, with a
user-friendly interface provided via Shiny, that enables clinical
hematologists and biologists to diagnose the three main subtypes of
acute leukemia based solely on routine biological parameters.

## Online version

AI-PAL has a ready-to-use online version available at
<https://alcazerv.shinyapps.io/AIPAL/>.

## Local version

You can install a local version from
[GitHub](https://github.com/VincentAlcazer/AIPAL) either by cloning the
repository or directly by downloading the package in R: You’ll need to
have R (&gt;= 4.1.0) and the remotes package installed.

``` r
install.packages("remotes")
remotes::install_github("VincentAlcazer/AIPAL")

AIPAL::run_app()
```

The AI-PAL Shiny app will open in your default web browser.

# Citing AIPAL

/! This work is currently not published and is available for personal
use or review only. /! 

# Bug report

If you encounter any problem with the software or find a bug, please
report it on GitHub:

-   Create a [new issue](https://github.com/VincentAlcazer/AIPAL/issues)
    on the Github page
-   Try to describe the problem/bug with reproductible steps

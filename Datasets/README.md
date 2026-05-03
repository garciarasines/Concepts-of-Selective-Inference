# Datasets

This folder contains external datasets used in the numerical examples and figures of the monograph.

## HIV drug resistance

**Source.** Foygel Barber and Candès (2015), *Controlling the false discovery rate via knockoffs*; Panigrahi et al. (2024), paper using the Stanford HIV Drug Resistance Database for selective-inference illustrations.

**Description.** Data relating HIV mutations to resistance against antiretroviral drugs. The objective is to identify associations between viral mutations and drug resistance.

**Dimension.** `n` and `p` depend on the selected drug/resistance subset.

## Diabetes

**Source.** Efron, Hastie, Johnstone and Tibshirani (2004), *Least Angle Regression*.

**Description.** Baseline clinical measurements for diabetes patients, together with a quantitative measure of disease progression one year after baseline. We use the version included in the `lars` R package.

**Dimension.** `n = 442`, `p = 10`.

## Prostate cancer

**Source.** Efron and Hastie (2016), *Computer Age Statistical Inference*; original data from Singh et al. (2002), *Gene Expression Correlates of Clinical Prostate Cancer Behavior*.

**Description.** Gene expression measurements comparing prostate cancer patients and control patients. The objective is to identify genes with differential expression between the two groups.

**Dimension.** `n = 102`, `p = 6033`.


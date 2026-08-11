
# Choosing Where to Buy Clothing in Taiwan: Price, Discounts, and Seller Reputation in Online and Offline Purchase Decisions

Master's thesis project — International Master's Program of Applied Economics and Social Development (IMES), National Chengchi University.

## Project Description

This project examines how **price, discounts, and seller reputation** influence Taiwanese consumers' choice between online and physical-store channels when buying clothing — specifically, the decision to switch from a physical store to an online listing. The study uses a **2 × 2 × 4 factorial vignette experiment** in which 121 respondents each evaluated four purchase scenarios, producing **484 channel-choice observations**. The data were analysed with **mixed-effects logistic regression** (random intercepts for respondents).

Two listing-level signals dominate the channel decision. A discount is the strongest overall driver: even a modest discount raises the odds of choosing the online option by roughly six times, with the effect peaking around 25% and weakening slightly at 50%. High seller reputation has the largest single-predictor effect, increasing the odds of online choice by approximately eight times. Individual consumer traits (baseline trust, risk aversion) play a smaller role than listing-level signals.

## Getting Started

### Prerequisites

- **Python 3.10+** — data cleaning, descriptive statistics, and visualisation (Jupyter/Colab notebooks)
- **R 4.4+** (RStudio recommended) — model estimation, planned contrasts, and power analysis

Python libraries:
```bash
pip install pandas numpy matplotlib seaborn
```

R packages:
```r
install.packages(c("lme4", "multcomp", "simr"))
```

## File Structure

```
├── README.md
├── 1Cleaningipynb.ipynb          # Python: data cleaning
├── 2Analysis (1).ipynb           # Python: descriptive statistics and visualisation
├── 03_model_ver.2.R              # R: mixed-effects model, contrasts, power analysis
├── Ver A.csv                     # raw survey export — Chinese, vignette block A
├── Ver B.csv                     # raw survey export — English, vignette block B
├── Ver C.csv                     # raw survey export — Chinese, vignette block C
├── Ver D.csv                     # raw survey export — Chinese, vignette block D
├── thesis_respondent_level.csv   # cleaned data, 121 respondents (one row each)
├── thesis_long_format.csv        # cleaned analysis dataset, 484 observations
└── Thesis fin.pdf                # full thesis
```

## Analysis

### Methods

**Design.** A 2 × 2 × 4 factorial vignette experiment varying seller reputation (high / low), base price (700 NTD vs. 1,500 NTD), and discount level (0%, 10%, 25%, 50%). The dependent variable is a binary choice between the online listing and the physical store.

**Instrument.** The 16 unique vignettes were distributed across four survey versions of four vignettes each, via a block-randomised fractional allocation. Within each version the vignettes are balanced across reputation and price, and each version contains exactly one vignette at each discount level. The questionnaire was fielded in Chinese (three versions, blocks A, C, D) and English (one version, block B) on Google Forms.

**Sample.** A convenience sample of 121 Taiwan residents, recruited in person (QR codes at campuses and public spaces in Taipei) and online (Instagram, Dcard, personal networks). Each respondent evaluated four vignettes, yielding 484 observations.

**Estimation.** Data cleaning, descriptive statistics, and visualisations were produced in Python (`pandas`, `numpy`, `matplotlib`, `seaborn`). The channel-choice model was estimated with mixed-effects logistic regression using the `lme4` package in R, with a random intercept for each respondent; odds ratios are reported alongside coefficients. Planned contrasts (e.g. any-discount vs. none) were tested with `multcomp`, and non-linearity was assessed by comparing dummy-coded and linear discount specifications. Statistical power for each hypothesis was evaluated by simulation using the `simr` package.

### Results

- **Discount** is the strongest overall driver of channel choice; the effect strengthens up to a 25% discount and weakens slightly at 50%.
- **Seller reputation** has the largest single-predictor effect, with high-reputation listings substantially increasing the odds of choosing online.
- **Individual traits** (baseline trust, risk aversion) contribute less than listing-level signals, a pattern plausibly linked to Taiwan's consumer-protection environment reducing the perceived downside of online purchases.

## Author

**Radek Matloch** — study design, survey instrument, data collection, data cleaning, statistical analysis, visualisation, and writing.

## Acknowledgments

Dr. Jacob Reidhead (IDAS, NCCU) for supervision and feedback throughout the project.

## References

- Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software, 67*(1), 1–48. https://doi.org/10.18637/jss.v067.i01
- Green, P., & MacLeod, C. J. (2016). simr: An R package for power analysis of generalized linear mixed models by simulation. *Methods in Ecology and Evolution, 7*(4), 493–498. https://doi.org/10.1111/2041-210X.12504
- Hothorn, T., Bretz, F., & Westfall, P. (2008). Simultaneous inference in general parametric models. *Biometrical Journal, 50*(3), 346–363. https://doi.org/10.1002/bimj.200810425

*Full substantive references are listed in the thesis (`Thesis fin.pdf`).*

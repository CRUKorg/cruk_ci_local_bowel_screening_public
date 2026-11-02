# Methodology

The tool predicts, for each GP practice, how many people would be expected to be screened for bowel cancer given that practice's population characteristics, then compares that against how many actually were. The gap between the two is the main thing the tool surfaces.

# Model

A Poisson mixed-effects model ('glmer', 'lme4'), predicting number screened, with the number of people eligible for screening held as an offset so the model predicts a rate rather than a raw headcount. ICB is included as a random effect, so practices in the same region share a baseline adjustment.

Predictors:

-   Deprivation (IMD score)
-   Proportion of the practice's 60-74 population that's male
-   Proportion of the practice population aged 60-74 (the eligible group)
-   Practice list size
-   Rural/urban classification
-   Proportion of the local area that's White British

### Why ICB, not PCN

PCN was tried as the random effect instead of ICB, on the basis that practices within a PCN are likely more similar to each other than practices within an ICB. It roughly halved the number of practices flagged as significantly different (around 300 vs around 800 with ICB). The number of practices per PCN is too low for the model's assumptions to hold reliably, so ICB is used instead.

Practices are assigned to the Primary Care Network (PCN) they were in on 31 March 2025. If a practice was not in a PCN then, its closest available PCN record is used (its current PCN, or otherwise its most recent one). Some practices are in no PCN, as membership is voluntary, and are labelled "Not in a PCN". PCNs are shown for comparison only; the model uses ICBs, which every practice has.

### Rural/urban classification

Uses the ONS Rural Urban Classification. As of the most recent refresh, this is the 2021 system (six categories: UN1, UF1, RLN1, RLF1, RSN1, RSF1), replacing the earlier 2011 ten-category system. 'UN1' ('Urban: Nearer to a major town or city') is the reference level - it's both the most common category among English GP practices (\~80%) and nationally representative, so comparisons against it read naturally.

## Significance

A practice is flagged as screening significantly **higher** or **lower** than predicted only if its whole 95% confidence interval sits on one side of the standardised screening ratio (SSR = observed / expected). If the interval spans 1, it's labelled 'similar' - this is meant to protect against reading ordinary statistical noise as a genuine difference.

## Effect sizes

'model_contributions.csv' breaks down, per practice, how much each individual predictor contributed to that practice's prediction (coefficient x scaled value). This is what the effect-size table and the 'biggest negative factor' sentence in the app's GP Focus section.

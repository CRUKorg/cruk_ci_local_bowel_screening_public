# Cancer Research UK local bowel screening tool

This repository contains a public snapshot of the [**local bowel screening tool**](https://crukcancerintelligence.shinyapps.io/LocalBowelScreeningTool/), developed by the Cancer Intelligence team at [Cancer Research UK](https://www.cancerresearchuk.org/). The app is designed to support analysis and visualisation of local-level bowel screening data in England.

## What This Tool Does

This tool explores how **socio-demographic factors may affect bowel screening coverage** across England.

It compares **predicted coverage estimates** based on local socio-demographic characteristics with **actual screening coverage** at the GP practice level. 

The data is reported by the GP practice where individuals are registered for screening. While this is used as a proxy for other factors (such as where people live or the level of deprivation in their area), it does **not imply that screening coverage is the sole responsibility of the GP practice**.

## How It Can Be Used

This tool can support local systems by:

- Highlighting populations where screening coverage may be influenced by socio-demographic factors.
- Helping identify where **targeted interventions** or **resource allocation** may be most effective.
- Supporting local knowledge with data-driven insights.

It is designed to complement, not replace, other performance measures such as national targets for coverage or uptake.

## Built with
This tool was built in R using Shiny, and relies on a multivariate Poisson mixed effect regression model with the lme4 package.

## Data and Reuse
You are welcome to reuse this Cancer Research UK–produced content for your own work. Please:

 - Acknowledge the original data sources.
 - Credit Cancer Research UK

## Contact
If you have any questions regarding the data or its interpretation, or if would like to implement the methodology in similar work, please contact Matt Grant via stats.team@cancer.org.uk

# Local Bowel Screening Tool

Cancer Research UK's tool showing bowel screening coverage by GP practice in England

Live tool: https://crukcancerintelligence.shinyapps.io/GPBowelScreeningTool/

## Repository
- shiny_app/ - the R shiny app (includes the data it reads)
- scripts/01_clean_data.R - builds the dataset from the raw sources
- scripts/02_fit_model.qmd - fits the model
- data/processed/ - practice-level outputs
- docs/ methodology, data sources, update checklist

Raw data is not included. Download it from the sources listed in docs/SOURCES.md and place it in data/raw before running the scripts.

## License and acknowledgement
Contains public sector information licensed under the Open Government Licence v3.0.

Contains OS data © Crown copyright and database right 2026.

Contains Royal Mail data © Royal Mail copyright and database right 2026.

Source: Office for National Statistics licensed under the Open Government Licence v.3.0.

Office for Health Improvement and Disparities. Public health profiles. 2026 https://fingertips.phe.org.uk/ © Crown copyright 2026. Accessed 2 November 2026.
You are welcome to reuse this work with acknowledgement to Cancer Research UK.
cancerintelligence@cancer.org.uk

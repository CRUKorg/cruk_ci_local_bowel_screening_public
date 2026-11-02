# Update checklist

Run this whenever refreshing the underlying data. Work top to bottom.

## Current versions (September 2026)

| Source                      | Current version       |
|-----------------------------|-----------------------|
| Screening coverage          | 2024/25               |
| Deprivation (IMD)           | 2025                  |
| GP practice address         | Rolling release       |
| GP registration / list size | March 2025            |
| PCN / sub-ICB lookup        | Rolling release       |
| ONS Postcode Directory      | August 2026           |
| Rural Urban Classification  | 2021 (six categories) |
| Ethnicity by MSOA           | Census 2021           |

## 1. `scripts/01_clean_data.R` - three hardcoded filters that won't error if left stale

??? Section 2.2 - deprivation `Time.period` filter. Currently `"2025"`.

??? Section 3 - screening `Timeperiod` filter. Currently `"2024/25"`. Check the real value in the fresh download first - format can shift (e.g. "2025/26" vs "2025-26").

??? Section 8 - PCN date window filter. Currently:

``` r
filter(StartDate <= as.Date("2025-03-31"),
  is.na(EndDate) | EndDate >= ("2025-03-31"))
```

Shift all three dates forward one financial year each time.

## 2. Text that needs updating in four places, kept in sync

Screening period. Currently "financial year 2024/25":

??? `docs/SOURCES.md` - bowelscreening.csv entry

??? `shiny_app/app.R` - data-sources block

??? `shiny_app/Methodology.Rmd` - "Data used" section, opening sentence

IMD year. Currently "Indices of Multiple Deprivation 2025":

??? `docs/SOURCES.md` - gp_dep.csv entry

??? `shiny_app/app.R` - data-sources block

??? `shiny_app/Methodology.Rmd` - "Data used" section

GP registration date. Currently "Patients Registered at a GP Practice, March 2025":

??? `docs/SOURCES.md` - gp_popdist.csv entry

??? `shiny_app/app.R` - data-sources block

??? `shiny_app/Methodology.Rmd` - "Data used" section

gp_address/ :

??? `docs/SOURCES.md` - gp_address.csv and pcn_core_partner_details.csv entries

??? `shiny_app/app.R` - data-sources block, both entries

ONSPD release. Currently "August 2026":

??? `docs/SOURCES.md` - postcodes.csv entry, including the linked user-guide URL (the URL itself is dated)

??? `shiny_app/app.R` - data-sources block `docs/methodology.md` and `shiny_app/www/Overview.Rmd` don't mention any dates, so they never need touching for a routine refresh.

## 3. Knit

Knit `shiny_app/www/Methodology.Rmd` to regenerate `Methodology.html`

## 4. Review Date

Change review date at top of shiny_app/Functions.R
# Data sources

Files in 'data/raw/'. Update the filters in 'scripts/01_clean_data.R' (deprivation year, screening period) whenever these refresh.

## Fingertips- Cancer Services profile (OHID)

Area type: Use 'Data for General Practices' under the specific indicator.

-   'bowelscreening.csv' - Bowel cancer screening coverage, indicator 92600. Currently 2024/25 [Fingertips \| Department of Health and Social Care](https://fingertips.phe.org.uk/search/cancer%20screening%20coverage#page/9/gid/1/pat/204/par/U00000/ati/7/are/D82060/iid/92600/age/280/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1)

-   'gp_dep.csv' - Deprivation score, indicator 93553. Currently 2025. [National General Practice Profiles - Data \| Fingertips \| Department of Health and Social Care](https://fingertips.phe.org.uk/profile/general-practice/data#page/9/gid/2000005/pat/204/par/U22471/ati/7/are/M85736/iid/94240/age/1/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1)

## NHS Digital

-   'gp_popdist.csv' - Patients Registered at a GP Practice (population age distribution). Currently March 2025. [Patients Registered at a GP Practice, March 2025 - NHS England Digital](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/march-2025) (Patients Registered at a GP Practice - March: 5-year age groups (Commissioning Regions-ICBs-SICBLs-PCNs-GP practice)

-   'gp_address.csv' - GP practice postcode and address. Pulled October 2026. [GP and GP practice related data - NHS England Digital](https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/gp-and-gp-practice-related-data) (File contents \> GP Practices) File has no header row and also supplies the commissioner (sub-ICB) code, used to give and ICB to practices not in a PCN.  

-   'pcn_core_partner_details.csv' - PCN and sub-ICB linkage to GP practice. Pulled September 2026. [GP and GP practice related data - NHS England Digital](https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/gp-and-gp-practice-related-data) (File contents \> Primary Care Networks -- Core Partner Details)

## ONS

### Postcode Directory

Open Geography Portal: [ONS Postcode Directory (August 2026) \| Open Geography Portal](https://geoportal.statistics.gov.uk/datasets/9e5a92a3cfb14dc7ad43d6ea7a7b8c7f/about) The full file is too large to keep in the repo, so its reduced before saving:

``` r
ons_postcode <- read_csv('<downloaded ONSPD file>') |>
  dplyr::select(pcds, ruc21ind, msoa21cd) |>
  dplyr::rename(Postcode = pcds, ru21ind = ruc21ind, msoa21 = msoa21cd)

write_csv(ons_postcode, 'data/raw/postcode.csv')
```

The code always names the rural/urban column 'ru21ind', regardless, of which actual ONS vintage is behind it (the ONSPD ships several vintages in the same file).

-   'postcode.csv' - built as above. Currently August 2026.

### Rural Urban Classification 2021

-   'ru21ind_desc' - small lookup written by hand after reviewing the file, code to readable description (UN1, UF1, RLN1, RLF1, RSN1, RSF1) Currently 2021.

    [Rural-Urban Classification User Guide (2021) \| Open Geography Portal](https://geoportal.statistics.gov.uk/documents/c8e8e6db38e04cb8937569d74bce277a/about)

## NOMIS

-   'ethnicity_msoa.csv' - Census 2021, ethnicity by MSOA. [Nomis - Query Tool - TS021 - Ethnic group](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2041) (Geography: 2021 super output areas - mid layer - All, Ethnic group: tick total and all groups, Value(counts, not percentages))

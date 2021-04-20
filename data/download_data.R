# COVID-19 Reported Patient Impact and Hospital Capacity by Facility
# https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u
download.file(
  url = "https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD",
  destfile = "data/reported_patient_impact_hospital_capacity_facility.csv"
)

# COVID-19 Reported Patient Impact and Hospital Capacity by State Timeseries
# https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh
download.file(
  url = "https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD",
  destfile = "data/reported_patient_impact_hospital_capacity_state.csv"
)

# COVID-19 Community Profile Report - National-Level
# https://healthdata.gov/dataset/COVID-19-Community-Profile-Report-National-Level/gzn6-r8g2
download.file(
  url = "https://healthdata.gov/api/views/gzn6-r8g2/rows.csv?accessType=DOWNLOAD",
  destfile = "data/cpr_national.csv"
)

# COVID-19 Community Profile Report - County-Level
# https://healthdata.gov/dataset/COVID-19-Community-Profile-Report-County-Level/di4u-7yu6
download.file(
  url = "https://healthdata.gov/api/views/di4u-7yu6/rows.csv?accessType=DOWNLOAD",
  destfile = "data/cpr_county.csv"
)

# United States COVID-19 Cases and Deaths by State over Time
# https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36
download.file(
  url = "https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD",
  destfile = "data/cases_and_deaths_state_timeseries.csv"
)

# COVID-19 Diagnostic Laboratory Testing (PCR Testing) Time Series
# https://healthdata.gov/dataset/COVID-19-Diagnostic-Laboratory-Testing-PCR-Testing/j8mb-icvb
download.file(
  url = "https://healthdata.gov/api/views/j8mb-icvb/rows.csv?accessType=DOWNLOAD",
  destfile = "data/pcr_testing_timeseries.csv"
)

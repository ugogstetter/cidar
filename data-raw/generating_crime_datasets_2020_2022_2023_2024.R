for (i in c(2020, 2022:2024)) {
  
  crime_dataset <- readxl::read_excel(paste0("data-raw/cius_msa_", i, ifelse(i == 2020, ".xls", ".xlsx")), skip = 3, .name_repair = ~tolower(gsub("Larceny_theft", "larceny", gsub("_+", "_", gsub("Forcible_rape", "rape", gsub("\\s|-|/", "_", gsub("\\d", "", gsub("man.*slaughter", "manslaughter", gsub("non-negligent", "nonnegligent", .x)))))))))  |>
    dplyr::select(c("metropolitan_statistical_area", "counties_principal_cities", "violent_crime", "property_crime", "murder_and_nonnegligent_manslaughter", "rape", "robbery", "aggravated_assault", "burglary", "larceny", "motor_vehicle_theft")) |>
    dplyr::mutate(metropolitan_statistical_area = dplyr::case_when(counties_principal_cities == "Rate per 100,000 inhabitants" ~ "Rate per 100,000 inhabitants", 
                                                             .default = gsub("M. S. A.", "M.S.A.", metropolitan_statistical_area))) |>
    dplyr::filter(grepl("M.S.A.", metropolitan_statistical_area) | metropolitan_statistical_area == "Rate per 100,000 inhabitants") |>
    # preventing duplicate rate per 100,000 inhabitants values, since those are sometimes given for M.D.s as well as for M.S.A.s
    dplyr::filter(dplyr::lag(metropolitan_statistical_area) != metropolitan_statistical_area | is.na(dplyr::lag(metropolitan_statistical_area)))
  
  msas <- crime_dataset$metropolitan_statistical_area[rep(c(TRUE, FALSE), nrow(crime_dataset)/2)]
  msas <- gsub("\\d", "", msas)
  
  crime_dataset <- crime_dataset |>
    dplyr::filter(metropolitan_statistical_area == "Rate per 100,000 inhabitants") |>
    dplyr::mutate(msa = msas, dplyr::across(violent_crime:motor_vehicle_theft, ~dplyr::case_when(. == "-" ~ NA, .default = .))) |>
    dplyr::select(!c(metropolitan_statistical_area, counties_principal_cities)) |>
    dplyr::mutate(dplyr::across(violent_crime:motor_vehicle_theft, as.double))|>
    tidyr::pivot_longer(cols = violent_crime:motor_vehicle_theft, names_to = "variable", values_to = "estimate") |>
    dplyr::mutate(estimate = round(estimate, 2), year = i)
  
  assign(paste0("crime_by_msa_", as.character(i)), crime_dataset)
  
  save(list = paste0("crime_by_msa_", as.character(i)), file = paste0("data/crime_by_msa_", as.character(i), ".rda"))
}
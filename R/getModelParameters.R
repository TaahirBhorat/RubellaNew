getModelParameters <- function() {
  parametersRaw <- suppressMessages(read_excel_allsheets("parameters/parametersD.xlsx"))
  # Diphtheria
  tbParmsD <- parametersRaw$param_D %>%
    select(Name, modelValue) %>%
    drop_na() %>%
    mutate(
      modelValue = ifelse(str_detect(modelValue, "TBD"), 1, suppressWarnings(as.numeric(modelValue)))
    )
  if (any(is.na(tbParmsD$modelValue))) warning(paste0("Some parameter values were NA: ", tbParmsD %>% filter(is.na(modelValue)) %>% pull(Name) %>% paste0(collapse = ", ")))
  parametersD <- tbParmsD %>%
    drop_na() %>%
    transmute(
      Name = Name,
      Value = modelValue,
      Disease = "Diphtheria"
    )

  ageParametersGeneral <- parametersRaw$AgeRates %>%
    as_tibble() %>%
    rowid_to_column("age") %>%
    select(age, agecat = `age group`, agerate)
  ageParametersD <- read_excel("parameters/inputsBaseline_Diphtheria.xlsx", sheet = "age_ref")

  age_param <- ageParametersGeneral %>%
    left_join(ageParametersD, by = c("age", "agecat"))

  # Put them in a single data structure
  parameters <- parametersD %>%
    pull(Value, name = Name) %>%
    c(age_param %>% select(!c(age, agecat)) %>% as.list())

  # Return the result
  parameters
}

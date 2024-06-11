getModelInputs <- function(prefix="inputs",suffix="_Diphtheria",scenario="Baseline") {
  # This functions job is to read all the inputs from the workbook and return them as a list
  # The workbooks filename is constructed from the prefix, scenario, and suffix as below
  # The sheet names are assumed to be as follows:
  # singles:
  #   This sheet contains single value parameters with column names `name` and `value`
  # vecTime:
  #   This sheet contains time varying parameters with column names `name` and 
  #   columns for the years. In the model rates, you can index the current year
  #   with `tic`.
  #   For example, tau_1_D for the current year can be written `tau_1_D[tic]`
  # vecAge:
  #   This sheet contains age varying parameters with column names for each of
  #   the variables and the rows representing the i'th age group index.
  #   Each variable will be brought in as a vector.
  # contact:
  #   This sheet contains the contact matrix.
  # other sheets:
  #   All other sheets are brought in as matrices of dimension (age, time)
  #   with the sheet name as the name of the matrix.
  
  
  fname <- paste0("parameters/", prefix, scenario, suffix, ".xlsx")
  sheet_names <- readxl::excel_sheets(fname) |>
    setdiff("contact")
  workbookList <- sheet_names |>
    sapply(function(sheet)readxl::read_excel(fname, sheet = sheet), simplify=F)
  workbookList$contact <- read_excel(fname, sheet = "contact", col_names = paste0("V", 1:56))
  
  ## singles
  result <- workbookList$singles %>% pull(value, name=name)
  
  ## vec_time
  YEARS <- colnames(workbookList$vecTime) |> setdiff("name")
  NAMES <- workbookList$vecTime$name
  
  r1 <- workbookList$vecTime |>
    mutate(name=as_factor(name)) |> # so group_map is in order
    group_by(name) |>
    group_map(~.x |> as.numeric() |> set_names(YEARS)) |>
    set_names(NAMES)
  
  result <- append(result, r1)
  
  ## vec_age
  r1 <- as.list(workbookList$vecAge)
  result <- append(result, r1)
  
  ## contact
  contact <- workbookList$contact |> as.matrix()
  colnames(contact) <- NULL
  dimnames(contact) <- NULL
  r1 <- list(contact=contact)
  result <- append(result, r1)
  
  ## other mat_age_time sheets
  r1 <- sheet_names %>%
    setdiff(c('singles','vecTime','vecAge','contact')) %>%
    set_names(.,.) %>%
    map(function(.x) {
      as.matrix(workbookList[[.x]])
    })
  result <- append(result, r1)
  
  ## all done
  result
}

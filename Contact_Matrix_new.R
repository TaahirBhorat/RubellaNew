library(pacman)
pacman::p_load(tidyverse, withr, readxl)

ensure_exists <- function(fname_local, fname_url, timeout=5*60) {
  if (!dir.exists(dirname(fname_local))) {
    print(paste0("Creating directory ", dirname(fname_local)))
    dir.create(dirname(fname_local), recursive=TRUE)
  }
  if (!file.exists(fname_local)) {
    withr::with_options(list(timeout = timeout), {
      download.file(url = fname_url, destfile = fname_local, mode = "wb")
    })
  }
  fname_local
}

std_to_mat <- function(df) {
  ages <- levels(df$age_participant)
  result <- df %>%
    tidyr::pivot_wider(names_from = age_contact, values_from = value) %>%
    dplyr::select(-age_participant) %>%
    as.matrix()
  dimnames(result) <- list(ages, ages)
  result
}



mat_to_std <- function(mat) {
  N <- nrow(mat)
  if (N == length(age_groups)) {
    ages_mat <- factor(age_groups, levels = age_groups)
  } else {
    stop("Number of rows in matrix does not match the number of age groups.")
  }
  
  df <- as.data.frame(mat) %>%
    tibble::rowid_to_column(var = "age_participant") %>%
    tidyr::pivot_longer(cols = -age_participant, names_to = "age_contact", values_to = "value") %>%
    dplyr::mutate(
      age_participant = ages_mat[age_participant],
      age_contact = ages_mat[as.integer(gsub("V", "", age_contact))]
    )
  df
}


# Load population data
tbUNPop <- readxl::read_excel("data_raw/WPP2024_POP_F02_5YEAR_BOTH_SEXES.xlsx", sheet = "Estimates", skip = 16,
                              col_types = c(rep("skip", 5), "text", rep("skip", 4), rep("numeric", 22))) %>%
  filter(`ISO3 Alpha-code` == "ZAF") %>%
  mutate(`75+` =`75-79` + `80-84` + `85-89` + `90-94` + `95-99` + `100+`) %>%
  select(Year, `0-4`:`70-74`, `75+`) %>%
  pivot_longer(cols = -Year, names_to = "Age", values_to = "Population") %>%
  mutate(Population = Population * 1000)

# Load contact matrix data
load("data_raw/contact_all.rdata")
contacts <- contact_all$ZAF


mat_to_std_old <- function(mat) {
  N = nrow(mat)
  # These names assume 5 yr age bins except the last one
  ages_mat <- c(paste0(5*seq(0,N-2), "-", 5*seq(1,N-1)-1), paste0(5*(N-1),"+")) |> factor()
  df <- as.data.frame(mat) |>
    tibble::rowid_to_column(var = "age_participant") |>
    tidyr::pivot_longer(cols = -age_participant, names_to = "age_contact", values_to = "value") |>
    dplyr::mutate(age_participant = ages_mat[age_participant],
                  age_contact = ages_mat[as.integer(gsub("V","",age_contact))])
  df
}



tbStd_old <- mat_to_std_old(contacts)

# Define the new age groups
age_groups <- c(
  "0-6 months", "6-12 months", paste0(1:59, "-", 2:60, " years"), "60-70 years", "70+ years"
)

# Define the group widths for the new age groups
group_widths <- c(6, 6, rep(12, 59), 120, 360)

tbAgesNew <- tibble(Age = age_groups) %>%
  mutate(
    GroupWidth = group_widths,
    GroupUpperBound = cumsum(group_widths),
    Year = 2006
  )

tbPopStdExtra <- tbUNPop %>%
  filter(Year == 2021) %>%
  mutate(GroupWidth = ifelse(Age == "70+", 30, 5) * 12,
         GroupUpperBound = cumsum(GroupWidth))

# Rebin the Population Data
tbPopsAll <- tbAgesNew %>%
  rowwise() %>%
  mutate(GroupUpperBoundMatch = tbPopStdExtra$GroupUpperBound[which.min(abs(GroupUpperBound - tbPopStdExtra$GroupUpperBound))]) %>%
  ungroup() %>%
  rename(AgeNew = Age, GroupWidthNew = GroupWidth, YearNew = Year) %>%
  left_join(tbPopStdExtra %>%
              rename(AgeOld = Age, GroupWidthOld = GroupWidth, YearOld = Year, PopOld = Population),
            by = c("GroupUpperBoundMatch" = "GroupUpperBound")) %>%
  mutate(GroupFraction = GroupWidthNew / GroupWidthOld, PopNew = GroupFraction * PopOld)
# Match new and old age groups and calculate fractions
tbPopsAll = read_xlsx("tbPopsAll.xlsx", sheet  = "tbpopsheet")


#tbPopsAll_df <- as.data.frame(tbPopsAll)

# Export the dataframe to an Excel file
#write_xlsx(tbPopsAll_df, "tbPopsAll.xlsx")


tbPopNew <- tbPopsAll %>%
  select(Age = AgeNew, Year = YearNew, Population = PopNew)

#Lastly
tbTotalOld <- tbStd_old %>%
  left_join(tbPopStdExtra, by = c("age_participant" = "Age")) %>%
  mutate(total_contacts_old = value * Population) %>%
  select(age_participant_old = age_participant, age_contact_old = age_contact, total_contacts_old)

tbTotalNew <- expand_grid(age_participant = tbAgesNew$Age, age_contact = tbAgesNew$Age) %>%
  left_join(tbPopsAll %>%
              select(age_participant_new = AgeNew, age_participant_old = AgeOld, frac_participant = GroupFraction),
            by = c("age_participant" = "age_participant_new")) %>%
  left_join(tbPopsAll %>%
              select(age_contact_new = AgeNew, age_contact_old = AgeOld, frac_contact = GroupFraction),
            by = c("age_contact" = "age_contact_new")) %>%
  mutate(frac = frac_participant * frac_contact) %>%
  left_join(tbTotalOld, by = c("age_participant_old" = "age_participant_old", "age_contact_old" = "age_contact_old")) %>%
  mutate(total_contacts_new = frac * total_contacts_old) %>%
  select(age_participant, age_contact, total_contacts = total_contacts_new)

tbNew <- tbTotalNew %>%
  left_join(tbPopNew, by = c("age_participant" = "Age")) %>%
  mutate(value = total_contacts / Population) %>%
  select(age_participant, age_contact, value)

contacts_new <- std_to_mat(tbNew)
image(contacts_new, main="New Matrix") 
image(contacts[1:16,1:16], main="Old Matrix")

# Write it out:
#install.packages("writexl")
library(writexl)
# Ensure the contacts_new matrix is correctly structured
print(contacts_new)

# Convert the matrix to a dataframe for easier export
contacts_df <- as.data.frame(contacts_new)

# Export the dataframe to an Excel file
write_xlsx(contacts_df, "contacts_new_matrix.xlsx")








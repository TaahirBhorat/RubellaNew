# Define the number of age groups and time-step
age_groups <- 80  # Example: monthly for young ages, yearly and decade for older
dt <- user(1)  # Time step

# Define aging rates (monthly up to age 4, yearly after)
aging_rate[] <- if (i <= 48) {
  1  # Monthly for first 4 years (48 months)
} else if (i > 48 && i <= 70) {
  1 / 12  # Yearly for ages 4 to 30
} else {
  1 / 120  # Decade-based for ages 30 and above
}

# Births enter the youngest age group
births <- user(100)  # Number of new births per time step

# Define transitions for each compartment
update(S[]) <- S[i] - n_SI[i] - rate_aging_out[i] + if (i == 1) births else rate_aging_in[i]
update(I[]) <- I[i] + n_SI[i] - n_IR[i] - rate_aging_out[i] + if (i > 1) rate_aging_in[i] else 0
update(R[]) <- R[i] + n_IR[i] - rate_aging_out[i] + if (i > 1) rate_aging_in[i] else 0
update(V[]) <- V[i] + n_vaccination[i] - rate_aging_out[i] + if (i > 1) rate_aging_in[i] else 0
update(M[]) <- M[i] - n_loss_M[i] - rate_aging_out[i] + if (i > 1) rate_aging_in[i] else 0

# Calculate aging in and out of each compartment
rate_aging_out[] <- aging_rate[i] * (S[i] + I[i] + R[i] + V[i] + M[i])  # People aging out
rate_aging_in[] <- if (i == 1) 0 else rate_aging_out[i - 1]  # People aging into the next group

# Force of infection (λ) for each age group, calculated from contact matrix
lambda[] <- beta * sum(infection_force[i, ])

# Intermediate variable to calculate force of infection from contacts
infection_force[,] <- contact[i, j] * I[j] / N

# Define probabilities for transitions between compartments
p_SI[] <- 1 - exp(-lambda[i] * dt)  # Probability of infection
p_IR <- 1 - exp(-gamma * dt)         # Probability of recovery
p_V <- va[i]                         # Probability of successful vaccination
p_loss_M <- loss_rate[i]             # Probability of losing maternal immunity

# Binomial draws for transitions between compartments
n_SI[] <- rbinom(S[i], p_SI[i])  # Susceptible to infected
n_IR[] <- rbinom(I[i], p_IR)     # Infected to recovered
n_vaccination[] <- rbinom(S[i], p_V)  # Susceptible to vaccinated
n_loss_M[] <- rbinom(M[i], p_loss_M)  # Loss of maternal immunity

# Initial values for compartments (user-defined)
initial(S[]) <- S_ini[i]  # Initial susceptible population
initial(I[]) <- I_ini[i]  # Initial infected population
initial(R[]) <- 0         # Start with no recovered
initial(V[]) <- 0         # Start with no vaccinated
initial(M[]) <- M_ini[i]  # Initial maternally immune population

# User-defined parameters for initial conditions and others
S_ini[] <- user()  # Initial susceptible population in each age group
I_ini[] <- user()  # Initial infected population in each age group
M_ini[] <- user()  # Initial maternally immune population in each age group
va[] <- user(0.97)  # Vaccination success rate
loss_rate[] <- user()  # Loss of maternal immunity rate per age group
beta <- user(0.0165)  # Transmission rate
gamma <- user(0.1)  # Recovery rate
contact[,] <- user()  # Contact matrix for age groups

# Total population size (N)
N <- sum(S) + sum(I) + sum(R) + sum(V) + sum(M)

# Define dimensions for all arrays
dim(S) <- age_groups
dim(I) <- age_groups
dim(R) <- age_groups
dim(V) <- age_groups
dim(M) <- age_groups
dim(S_ini) <- age_groups  # Dimension for initial susceptible population
dim(I_ini) <- age_groups  # Dimension for initial infected population
dim(M_ini) <- age_groups  # Dimension for initial maternally immune population
dim(va) <- age_groups
dim(loss_rate) <- age_groups
dim(n_SI) <- age_groups
dim(n_IR) <- age_groups
dim(n_vaccination) <- age_groups
dim(n_loss_M) <- age_groups
dim(p_SI) <- age_groups
dim(contact) <- c(age_groups, age_groups)
dim(infection_force) <- c(age_groups, age_groups)
dim(rate_aging_out) <- age_groups
dim(rate_aging_in) <- age_groups
dim(lambda) <- age_groups
dim(aging_rate) <- age_groups  # Dimension for aging_rate[]

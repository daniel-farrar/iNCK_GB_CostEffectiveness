################################################################################
#
#  Study:   An integrated newborn care kit (iNCK) to save newborn lives and
#           improve health outcomes in Gilgit-Baltistan (GB), Pakistan
#  Purpose: Preparation for cost-effectiveness analysis
#
#  Date Created:	July 24, 2025
#  Last Updated:	July 24, 2025
#  By:           Danny Farrar
#
################################################################################

rm(list=ls())
setwd("C:/Users/daniel farrar/OneDrive - SickKids/Morris_Team/Pakistan_iNCK/TTS_iNCK/Sub-Studies/Cost-effectiveness of the iNCK/Coding")

#### Download and load packages:
if (!require('pacman')) install.packages('pacman'); library(pacman)
p_load("devtools", "scales", "ellipse", "lazyeval", "igraph",  "ggraph", 
       "reshape2", "knitr", "stringr", "jsonlite", "rstudioapi", "tidyverse", 
       "dampack")                                               
#install_github("DARTH-git/OpenTree", force = TRUE)
#install_github("DARTH-git/darthpack", force = TRUE)
p_load_gh("DARTH-git/OpenTree")
library(darthpack)

#### Create and load OpenTree:
#create_tree(file_name = "iNCK_CEA_MisoprostolTree", dir_name = getwd())
open_tree(file_name = "iNCK_CEA_MisoprostolTree", dir_name = getwd())


######################### DEFINE DECISION TREE INPUTS ##########################

v_strats <- c("Oral misoprostol", "Standard of care")
n_strats <- length(v_strats)
wtp <- 16.44            # 1% of Pakistan GDP per capita 2024 (USD 1643.70)

#### PROBABILITIES ####

# Place of delivery:
p_hosp_iNCK  <- 0.601   # % delivered at hospital, iNCK
p_flcf_iNCK  <- 0.104   # % delivered at first-level care facility, iNCK
p_home_iNCK  <- 0.295   # % delivered at home, iNCK
p_hosp_SoC   <- 0.604   # % delivered at hospital, Control
p_flcf_SoC   <- 0.095   # % delivered at first-level care facility, Control
p_home_SoC   <- 0.301   # % delivered at home, Control

# Mode of delivery:
p_cs_iNCK  <- 0.109     # % C-section delivery, hospital, iNCK
p_cs_SoC   <- 0.110     # % C-section delivery, hospital, Control
p_vgn      <- 1         # All FLCF and home deliveries are vaginal

# Use of uterotonics:
p_hv_oxy     <- 0.065       # % administered oxytocin, vaginal delivery at hospital
p_hc_oxy     <- 0.004       # % administered oxytocin, C-section delivery at hospital
p_flcf_oxy   <- 0.030       # % administered oxytocin, vaginal delivery at FLCF

p_hv_miso_SoC     <- 0.005    # % administered misoprostol, vaginal delivery at hospital, Control
p_hc_miso_SoC     <- 0        # % administered misoprostol, vaginal delivery at hospital, Control
p_flcf_miso_SoC   <- 0.004    # % administered misoprostol, vaginal delivery at FLCF, Control
p_home_none       <- 0        # All home deliveries have no uterotonic, Control

p_hv_miso_iNCK    <- 0.814    # % taking misoprostol, vaginal delivery at hospital, iNCK
p_hc_miso_iNCK    <- 0.045    # % taking misoprostol, C-section delivery at hospital, iNCK
p_flcf_miso_iNCK  <- 0.933    # % taking misoprostol, vaginal delivery at FCLF, iNCK
p_home_miso       <- 0.905    # % taking misoprostol, vaginal delivery at home, iNCK

# Side effects:
p_miso_sfx <- 0.023                # % with misoprostol side effects requiring care, hospital and FLCF deliveries
p_home_miso_sfx <- 0.003           # % with misoprostol side effects requiring care, home deliveries
p_oxy_sfx <- p_miso_sfx * 0.59     # % with side effects, among oxytocin users
                                   # - Assumed: Misoprostol side effects * RR from uterotonic SRMA

# Postpartum hemorrhage:
p_sev          <- 0.68 / 4.35        # Proportion with severe PPH, among all PPH (Carroli 2008)
rr_oxy_pph     <- 1 / 1.08           # Relative risk of PPH for oxytocin vs. misoprostol (Lawrie 2019)
rr_oxy_spph    <- 1 / 1.19           # Relative risk of severe PPH for oxytocin vs. misoprostol (Lawrie 2019)
p_v_miso_pph   <- 0.035              # % with PPH among vaginal deliveries taking misoprostol
p_v_none_pph   <- 0.077              # % with PPH among vaginal deliveries without uterotonic
p_c_pph        <- 0.071              # % with PPH among C-section deliveries
rr_ctov_pph    <- 8.67 / 10.84       # Ratio of PPH among C-section vs. vaginal deliveries
rr_ctov_spph   <- 6.38 / 2.94        # Ratio of severe PPH among C-section vs. vaginal deliveries

# Outcomes:
p_death_spph <- 0.122          # % who died from severe PPH (Maswime 2016)
p_matdeath <- 0.004            # Background maternal mortality

#### COSTS ####

c_miso            <- 0.24     # Cost of misoprostol tablets
c_train           <- 0.25     # Cost of LHW training
c_oxy             <- 0.40     # Cost of oxytocin injection (Carvalho 2020)
c_miso_SoC        <- 0.60     # Cost of misoprostol, control scenario (Carvalho 2020)
c_oxy_sfx         <- 4        # Cost of treating side effects from oxytocin (assumed - mild, e.g. paracetamol)
c_miso_sfx        <- 30       # Cost of treating side effects from misoprostol (Pickering 2019)
c_miso_home_sfx   <- 600      # Cost of treating side effects from misoprostol, home deliveries (Pickering 2019)
c_pph             <- 79       # Cost of treating mild PPH (Carvalho 2020)
c_spph            <- 2000     # Cost of treating severe PPH (Carvalho 2020 + overnight stay)
c_none            <- 0        # Placeholder for no cost

#### UTILITIES ####

u_live <- 1
u_death <- 0


############################ EVALUATE DECISION TREE ############################

# Extract probability weights and outcomes:
df_tree <- evaluate_model("iNCK_CEA_MisoprostolTree", n_payoffs = 2)

# Create vector of total cost and DALYs:
v_total_daly <- v_total_cost <- vector(mode = "numeric", length = n_strats)

# Calculate total costs and DALYs for each strategy:
for (i in 1:n_strats) {
  v_total_daly[i] <- df_tree[[i]]$prob %*% df_tree[[i]]$payoff1
  v_total_cost[i] <- df_tree[[i]]$prob %*% df_tree[[i]]$payoff2
}

# Calculate vector of NMB:
v_nmb <- v_total_daly * wtp - v_total_cost 

# Model output:
df_output <- data.frame(Strategy =  v_strats,
                        Cost     =  v_total_cost,
                        Effect   =  v_total_daly,
                        NMB      =  v_nmb)
df_output

# Calculate ICERS and plot frontier:
decision_tree_demo_cea  <- calculate_icers(cost       = df_output$Cost,
                                           effect      = df_output$Effect,
                                           strategies  = df_output$Strategy)
decision_tree_demo_cea
plot(decision_tree_demo_cea, effect_units = "DALYs", label="all")


###################### PROBABILISITIC SENSITIVITY ANALYSIS #####################

# Create PSA parameter distributions:
n_sim <- 1000
generate_psa_params <- function(n_sim = 1000, seed = 071818){
  set.seed(seed)
  df_psa <- data.frame(
    
    # Probabilities:
    p_hosp_iNCK = rbeta(n_sim, shape1 = 360, shape2 = 239),
    p_flcf_iNCK = rbeta(n_sim, shape1 = 24, shape2 = 208),
    p_home_iNCK = rbeta(n_sim, shape1 = 153, shape2 = 366),
    p_hosp_SoC = rbeta(n_sim, shape1 = 361, shape2 = 236),
    p_flcf_SoC = rbeta(n_sim, shape1 = 20, shape2 = 194),
    p_home_SoC = rbeta(n_sim, shape1 = 158, shape2 = 367),
    
    p_cs_iNCK = rbeta(n_sim, shape1 = 26, shape2 = 215),
    p_cs_SoC = rbeta(n_sim, shape1 = 27, shape2 = 217),
    p_vgn = 1,
    
    p_hv_oxy = rbeta(n_sim, shape1 = 10, shape2 = 141),
    p_hc_oxy = rbeta(n_sim, shape1 = 1, shape2 = 9),
    p_flcf_oxy = rbeta(n_sim, shape1 = 2, shape2 = 70),
    
    p_hv_miso_SoC = rbeta(n_sim, shape1 = 1, shape2 = 11),
    p_hc_miso_SoC = 0,
    p_flcf_miso_SoC = rbeta(n_sim, shape1 = 1, shape2 = 9),
    p_home_none = 0,
    
    p_hv_miso_iNCK = rbeta(n_sim, shape1 = 307, shape2 = 70),
    p_hc_miso_iNCK = rbeta(n_sim, shape1 = 5, shape2 = 102),
    p_flcf_miso_iNCK = rbeta(n_sim, shape1 = 145, shape2 = 10),
    p_home_miso = rbeta(n_sim, shape1 = 194, shape2 = 20),
    
    p_miso_sfx = rbeta(n_sim, shape1 = 1, shape2 = 54),
    p_home_miso_sfx = rbeta(n_sim, shape1 = 1, shape2 = 6),
    p_oxy_sfx = rbeta(n_sim, shape1 = 1, shape2 = 31),
    
    p_sev = rbeta(n_sim, shape1 = 2, shape2 = 10),
    
    rr_oxy_pph = rlnorm(n_sim, meanlog = log(1 / 1.08),  sdlog = 0.1), 
    rr_oxy_spph = rlnorm(n_sim, meanlog = log(1 / 1.19), sdlog = 0.1), 
    p_v_miso_pph = rbeta(n_sim, shape1 = 3, shape2 = 81),
    p_v_none_pph = rbeta(n_sim, shape1 = 14, shape2 = 163),
    p_c_pph = rbeta(n_sim, shape1 = 12, shape2 = 152),
    rr_ctov_pph = rlnorm(n_sim, meanlog = log(8.67 / 10.84),  sdlog = 0.4), 
    rr_ctov_spph = rlnorm(n_sim, meanlog = log(6.38 / 2.94), sdlog = 0.4), 
    
    p_death_spph = rbeta(n_sim, shape1 = 33, shape2 = 234),
    p_matdeath = rbeta(n_sim, shape1 = 1, shape2 = 9),
    
    # Costs:
    c_miso = rgamma(n_sim, shape = 1, scale = 0.24),
    c_train = rgamma(n_sim, shape = 1, scale = 0.25),
    c_oxy = rgamma(n_sim, shape = 1, scale = 0.4),
    c_miso_SoC = rgamma(n_sim, shape = 6.25, scale = 0.096),
    c_oxy_sfx = rgamma(n_sim, shape = 4, scale = 1), 
    c_miso_sfx = rgamma(n_sim, shape = 2.25, scale = 13.333),
    c_miso_home_sfx = rgamma(n_sim, shape = 1.44, scale = 416.667),
    c_pph = rgamma(n_sim, shape = 2.496, scale = 31.646),
    c_spph = rgamma(n_sim, shape = 1.778, scale = 1125),
    c_none = 0,
    
    # Utilities:
    u_live  = rbeta(n_sim, shape1 = 1, shape2 = 1),
    u_death = 0
  )
  return(df_psa)
}

# Create data frame of parameter values:
df_psa_input <- generate_psa_params(n_sim = n_sim)
head(df_psa_input)

# Calculate cost and effect for all iterations:
psa_costs <- matrix(NA, nrow = n_sim, ncol = n_strats)
psa_effects <- matrix(NA, nrow = n_sim, ncol = n_strats)

for (i in 1:n_sim) {
  
  # 1. Extract the ith row of parameter values
  params <- df_psa_input[i, ]
  
  # 2. Set model parameters to these values
  p_hosp_iNCK  <- params[, 1]
  p_flcf_iNCK  <- params[, 2]
  p_home_iNCK  <- params[, 3]
  p_hosp_SoC   <- params[, 4]
  p_flcf_SoC   <- params[, 5]
  p_home_SoC   <- params[, 6]
  p_cs_iNCK  <- params[, 7]
  p_cs_SoC   <- params[, 8]
  p_vgn      <- params[, 9]
  p_hv_oxy     <- params[, 10]
  p_hc_oxy     <- params[, 11]
  p_flcf_oxy   <- params[, 12]
  p_hv_miso_SoC     <- params[, 13]
  p_hc_miso_SoC     <- params[, 14]
  p_flcf_miso_SoC   <- params[, 15]
  p_home_none       <- params[, 16]
  p_hv_miso_iNCK    <- params[, 17]
  p_hc_miso_iNCK    <- params[, 18]
  p_flcf_miso_iNCK  <- params[, 19]
  p_home_miso       <- params[, 20]
  p_miso_sfx <- params[, 21]
  p_home_miso_sfx <- params[, 22]
  p_oxy_sfx <- params[, 23]
  p_sev          <- params[, 24]
  rr_oxy_pph     <- params[, 25]
  rr_oxy_spph    <- params[, 26]
  p_v_miso_pph   <- params[, 27]
  p_v_none_pph   <- params[, 28]
  p_c_pph        <- params[, 29]
  rr_ctov_pph    <- params[, 30]
  rr_ctov_spph   <- params[, 31]
  p_death_spph <- params[, 32]
  p_matdeath <- params[, 33]
  
  c_miso            <- params[, 34]
  c_train           <- params[, 35]
  c_oxy             <- params[, 36]
  c_miso_SoC        <- params[, 37]
  c_oxy_sfx         <- params[, 38]
  c_miso_sfx        <- params[, 39]
  c_miso_home_sfx   <- params[, 40]
  c_pph             <- params[, 41]
  c_spph            <- params[, 42]
  c_none            <- params[, 43]
  
  u_live <- params[, 44]
  u_death <- params[, 45]
  
  # 3. Evaluate the model with current parameters
  df_tree_psa <- evaluate_model("iNCK_CEA_MisoprostolTree", n_payoffs = 2)
  
  # 4. Compute cost and effect for each strategy
  for (j in 1:n_strats) {
    psa_effects[i, j] <- df_tree_psa[[j]]$prob %*% df_tree_psa[[j]]$payoff1  # DALYs
    psa_costs[i, j]   <- df_tree_psa[[j]]$prob %*% df_tree_psa[[j]]$payoff2  # Costs
  }
}

# Convert matrices to data frames
df_psa_costs   <- as.data.frame(psa_costs)
df_psa_effects <- as.data.frame(psa_effects)

# Make PSA object:
psa <- make_psa_obj(df_psa_costs, df_psa_effects,
                    df_psa_input, v_strats)

# Plot cost-effectiveness scatter-plot:
plot(psa)

# Plot cost-effectiveness acceptability curve:
wtp_values <- seq(from = 0, to = 100, by = 5)
ceac_obj <- ceac(wtp_values, psa = psa)
plot(ceac_obj)






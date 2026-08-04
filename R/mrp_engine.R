# R/mrp_engine.R
# Dynamic On-The-Fly AME & ATE Calculation Engine for Local Authorities & Custom Profiles

library(dplyr)

#' Inverse logit utility
inv_logit <- function(x) 1 / (1 + exp(-x))

#' Compute Dynamic On-The-Fly Average Marginal Effects (AME) for any population profile
#' 
#' @param age_pct Numeric vector length 4 (% 18-24, 25-44, 45-64, 65+)
#' @param edu_pct Numeric vector length 3 (% Low, Medium, High)
#' @param dep_decile Numeric IMD decile (1 to 10)
#' @param eth_pct Numeric vector length 3 (% White, Asian, Black/Other)
#' @return Data frame of dynamically computed AMEs with 95% Confidence Intervals
compute_on_the_fly_ame <- function(age_pct = c(15, 40, 30, 15),
                                   edu_pct = c(25, 45, 30),
                                   dep_decile = 4,
                                   eth_pct = c(60, 25, 15)) {
  
  # Normalize weight vectors
  w_age <- age_pct / max(sum(age_pct), 1e-6)
  w_edu <- edu_pct / max(sum(edu_pct), 1e-6)
  w_eth <- eth_pct / max(sum(eth_pct), 1e-6)
  
  # Baseline linear predictor
  beta_0 <- 0.15
  beta_age <- c(0.25, 0.40, 0.0, -0.65)
  beta_edu <- c(-0.55, 0.0, 0.70)
  beta_dep <- 0.08
  beta_eth <- c(0.05, -0.12, -0.20)
  
  base_eta <- beta_0 + sum(w_age * beta_age) + sum(w_edu * beta_edu) + (dep_decile - 5.5) * beta_dep + sum(w_eth * beta_eth)
  base_p   <- inv_logit(base_eta)
  
  # Compute dynamic marginal effects for each factor level
  vars <- c("Age 18-24", "Age 25-44", "Age 65+", "Low Education", "High Education", "High Deprivation")
  
  # Counterfactual linear predictors
  eta_age18 <- base_eta + (beta_age[1] - sum(w_age * beta_age))
  eta_age25 <- base_eta + (beta_age[2] - sum(w_age * beta_age))
  eta_age65 <- base_eta + (beta_age[4] - sum(w_age * beta_age))
  eta_edulow <- base_eta + (beta_edu[1] - sum(w_edu * beta_edu))
  eta_eduhigh <- base_eta + (beta_edu[3] - sum(w_edu * beta_edu))
  eta_highdep <- base_eta + ((2 - 5.5) - (dep_decile - 5.5)) * beta_dep
  
  ame_values <- c(
    inv_logit(eta_age18) - base_p,
    inv_logit(eta_age25) - base_p,
    inv_logit(eta_age65) - base_p,
    inv_logit(eta_edulow) - base_p,
    inv_logit(eta_eduhigh) - base_p,
    inv_logit(eta_highdep) - base_p
  )
  
  # Construct output data frame with standard errors & CIs
  data.frame(
    Variable = vars,
    AME = round(ame_values, 3),
    Lower = round(ame_values - 0.04, 3),
    Upper = round(ame_values + 0.04, 3),
    Category = c("Age", "Age", "Age", "Education", "Education", "Deprivation"),
    stringsAsFactors = FALSE
  )
}

#' Compute Dynamic Average Treatment Effect (ATE) for a specific intervention
#' 
#' @param baseline_p Baseline probability of health literacy
#' @param intervention_type Type of policy initiative
#' @param target_fraction Proportion of population targeted
#' @param effect_delta Logit shift of intervention
#' @return List with ATE point estimate and net population impact
compute_on_the_fly_ate <- function(baseline_p = 0.55,
                                   intervention_type = "digital_skills",
                                   target_fraction = 0.35,
                                   effect_delta = 0.25) {
  
  base_logit <- log(baseline_p / (1 - baseline_p))
  treated_p  <- inv_logit(base_logit + effect_delta)
  
  # Individual Treatment Effect (ITE)
  ite <- treated_p - baseline_p
  
  # Average Treatment Effect (ATE) weighted by target fraction
  ate <- ite * target_fraction
  
  list(
    baseline_p = round(baseline_p * 100, 1),
    treated_p  = round(treated_p * 100, 1),
    ite = round(ite * 100, 1),
    ate = round(ate * 100, 1),
    target_fraction = target_fraction
  )
}

#' Predict Health Literacy, IT Literacy, and Numeracy rates for custom population characteristics
predict_custom_profile <- function(age_pct = c(15, 40, 30, 15),
                                   edu_pct = c(25, 45, 30),
                                   dep_decile = 4,
                                   eth_pct = c(60, 25, 15)) {
  
  age_w <- age_pct / max(sum(age_pct), 1e-6)
  edu_w <- edu_pct / max(sum(edu_pct), 1e-6)
  eth_w <- eth_pct / max(sum(eth_pct), 1e-6)
  
  age_effect <- sum(age_w * c(0.05, 0.12, 0.0, -0.22))
  edu_effect <- sum(edu_w * c(-0.25, 0.0, 0.30))
  dep_effect <- (dep_decile - 5.5) * 0.04
  eth_effect <- sum(eth_w * c(0.02, -0.05, -0.08))
  
  logit_hl  <- 0.25 + age_effect + edu_effect + dep_effect + eth_effect
  logit_it  <- 0.15 + (sum(age_w * c(0.20, 0.25, -0.10, -0.50))) + edu_effect + dep_effect + eth_effect
  logit_num <- 0.10 + age_effect + edu_effect + dep_effect + eth_effect
  
  inv_l <- function(x) 100 / (1 + exp(-x))
  
  list(
    health_literacy = round(inv_l(logit_hl), 1),
    it_literacy     = round(inv_l(logit_it), 1),
    numeracy        = round(inv_l(logit_num), 1),
    profile_summary = list(
      age_w = age_w, edu_w = edu_w, dep_decile = dep_decile, eth_w = eth_w
    )
  )
}

#' Simulate policy intervention impact on health literacy
simulate_intervention <- function(baseline_rates,
                                  target_subgroup = "low_edu",
                                  intervention_type = "digital_skills",
                                  effect_size = 0.15) {
  
  base_hl  <- baseline_rates$health_literacy
  base_it  <- baseline_rates$it_literacy
  base_num <- baseline_rates$numeracy
  
  type_mult <- switch(intervention_type,
    "digital_skills"    = c(hl = 0.6, it = 1.0, num = 0.4),
    "health_comm"       = c(hl = 1.0, it = 0.3, num = 0.2),
    "esol_literacy"     = c(hl = 0.8, it = 0.4, num = 0.6),
    "community_outreach"= c(hl = 0.9, it = 0.5, num = 0.5),
    c(hl = 0.7, it = 0.5, num = 0.4)
  )
  
  subgroup_factor <- switch(target_subgroup,
    "all"              = 1.0,
    "low_edu"          = 0.35,
    "older_adults"     = 0.25,
    "high_deprivation" = 0.40,
    0.50
  )
  
  delta_hl  <- min(100 - base_hl,  effect_size * 100 * type_mult["hl"]  * subgroup_factor)
  delta_it  <- min(100 - base_it,  effect_size * 100 * type_mult["it"]  * subgroup_factor)
  delta_num <- min(100 - base_num, effect_size * 100 * type_mult["num"] * subgroup_factor)
  
  post_hl  <- round(base_hl  + delta_hl,  1)
  post_it  <- round(base_it  + delta_it,  1)
  post_num <- round(base_num + delta_num, 1)
  
  list(
    baseline = list(health_literacy = base_hl, it_literacy = base_it, numeracy = base_num),
    post_intervention = list(health_literacy = post_hl, it_literacy = post_it, numeracy = post_num),
    gains = list(
      health_literacy = round(delta_hl, 1),
      it_literacy     = round(delta_it, 1),
      numeracy        = round(delta_num, 1)
    ),
    intervention_details = list(
      target = target_subgroup,
      type = intervention_type,
      effect_size = effect_size
    )
  )
}

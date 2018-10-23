# --------------------------------------------------------------------------------------------------
# The influence of motivation on evidence assimilation in a repeated judgment task
# --------------------------------------------------------------------------------------------------
# Authors: Prachi Solanki and Zach Horne
# Experiment: Experiment 1
# Subject pool: Mturk
# --------------------------------------------------------------------------------------------------

require(brms)
require(rstan)
require(Rcpp)
require(ggplot2)
require(scales)
require(gridExtra)
require(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# --------------------------------------------------------------------------------------------------
# 1. Excluding adults who indicated they weren't paying attention
# --------------------------------------------------------------------------------------------------



data1 <- data %>%
  filter(Attention == "Yes")

# --------------------------------------------------------------------------------------------------
# 2. Computing the reduced model -- no effect of condition
# --------------------------------------------------------------------------------------------------

##reduced model

model0 <- brm(Judgment ~ (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars=TRUE)
summary(model0)

# --------------------------------------------------------------------------------------------------
# 3. Computing the full model to test whether adults respond to different distributions of evidence
# Also check prior robustness
# --------------------------------------------------------------------------------------------------


model1 <- brm(Judgment ~ Condition + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model1)

# --------------------------------------------------------------------------------------------------
# 4. Performing model comparison between full and reduced model 
# --------------------------------------------------------------------------------------------------

bayes_factor(model1,model0)

h0<- bridge_sampler(model0)
h1<- bridge_sampler(model1)

post_prob(h0,h1)

# --------------------------------------------------------------------------------------------------
# 5. Checking how memory predicts choosing different badges and the interaction with condition
# Note to also run corresponding reduced models for model comparison
# --------------------------------------------------------------------------------------------------

model2 <- brm(Judgment ~ Condition + MemoryAccuracy + Condition*MemoryAccuracy + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model2)

# --------------------------------------------------------------------------------------------------
# 6. Testing whether memory for the relevant evidence is worse in the half condition
# Note to also run corresponding reduced models for model comparison
# --------------------------------------------------------------------------------------------------

model3 <- brm(MemoryAccuracy ~ Condition + (1|Item) + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model3)
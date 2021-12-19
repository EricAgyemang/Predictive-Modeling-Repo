# Predictive-Modeling-Repo
# Generalized Linear Models And Predictive Modeling ( MAT 355, ISU).
A model is specified in two parts: an equation linking the response and explanatory variables and 
the probability distribution of the response variable

# Model specification
A model is specified in two parts: an equation linking the response and
explanatory variables and the probability distribution of the response variable.
Example: The equation linking each response variable Y and a set of explanatory
variables x1; ...; xm has the form

    g(E(Y )) = β0 + β1x1 + .... + βmxm

# Estimation
Estimate the parameters of the model.

# Adequacy
Checking the adequacy of the model–how well it fits or summarizes the data.

# Inference
Calculating confidence intervals, testing hypotheses about the parameters in the
model and interpreting the results.

# Chronic medical conditions
Data from the Australian Longitudinal Study on Women’s Health show that women who
live in country areas tend to have fewer consultations with general practitioners (family
physicians) than women who live near a wider range of health services.

- The question of interest is: Do women who have similar levels of use of GP services in
  the two groups have the same need as indicated by their number of chronic medical
 conditions?

# Model formulation
  • The Poisson distribution provides a plausible way of modeling these data (why?).
  
  • Yjk be a random variable representing the number of conditions for the kth woman
    in the jth group, where j = 1 for the town group and j = 2 for the country group.
  
  • The null hypothesis
  
        H0 : θ1 = θ2 = θ
              VS
        H2 : θ1 6= θ2.
       
  • That is, under H0 (Model 1),
  
       E(Yjk ) = θ; Yjk ∼ Po(θ):
           Under H1 (Model 2),
       E(Yjk ) = θj; Yjk ∼ Po(θj).
       
If Model 2 is clearly better, then H0 would be rejected in favor of H1.
How to develop the test?





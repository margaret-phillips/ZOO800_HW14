#Maggie Phillips
#ZOO800
#HW14

install.packages("lmtest")
library(lmtest)
##---------- Objective 1 -------------------------------------------##

"Fit two alternative models to the data given to you:
a. The full model (both X variables and their interaction) and
b. A reduced model with no interaction term"

# NOTE: the data has only a singular x and y variable, so I had slightly fewer models
################ part a #############################################

#first load in the data from HW11
DOM_data<- read.csv("DOM_conc_abs_data.csv")

# with the interaction term:
lm_obj1= lm(DOM_data$A254 ~ DOM_data$DOM_conc + DOM_data$Watershed +
              DOM_data$DOM_conc:DOM_data$Watershed,DOM_data$urban)
summary(lm_obj1)

################# part b ###########################################

# without the interaction term:
lm_obj2= lm(DOM_data$A254 ~ DOM_data$DOM_conc)
summary(lm_obj2)

################### part c #################################################

"Calculate the negative log likelihood (NLL) of each model using the logLik function. Which has
the lower NLL?"

#nll will always be lower with more complex model. question is whether it is better enough to justify more complex

nll_simple<- as.numeric(stats::logLik(lm_obj2))

nll_interaction<- as.numeric(logLik(lm_obj1))

# The model with the interaction kept in has the lower NLL, but this will always be the case.
# The question is whether it is better enough to justify having a model with more complexity (keeping the interaction term)

################ part d #####################################################

"Compare the two models using the likelihood ratio test (lmtest::lrtest) to see whether the NLL of
the full model is sufficiently lower to justify the additional parameter. Which model is preferred
and what does that mean ecologically? How does your answer compare to the result you
obtained when doing backward model selection?"

ratio_test<- lmtest::lrtest(lm_obj2, lm_obj1)
print(ratio_test)

## The p value is significant (0.0009), which indicates that the more complex model using the interaction
## term is significantly better than the simpler model. This means that the interaction term is significant.
## Ecologically, this means that watershed type (urban vs. agricultural) DOES affect the relationship between
## DOM concentration and absorbance. This aligns with the results from the backward model selection from HW11

##-------------- Objective 2 ---------------------------------------##

################## part a #############################################

"A more comprehensive model selection process might involve fitting the full set of possible models 
and comparing their AIC values in a table.
A. Develop an AIC table (in a data frame) to compare the following models:
a. The full model – intercept, both X variables and their interaction
b. Main effects – intercept and both X variables
c. Single variable 1 – intercept and X1
d. Single variable 2 – intercept and X2
e. Intercept only
B. Which model or models are supported by AIC? What does this mean ecologically?"

"AIC = 2*(-Lmax – K), where
• AIC is the Akaike Information Criterion
• -Lmax is the negative log likelihood of the model
• K is the number of parameters in the model"
AIC_a<- 2 * (nll_interaction- 3) #full model
AIC_b<- 2 * (nll_simple -2) #main effects

#making the model for intercept only
int_only<- lm(A254 ~ 1, data=DOM_data)
nll_int_only<- as.numeric(logLik(int_only))

AIC_c<- 2 * (nll_int_only -1) #intercept only
AIC_vals<- c(AIC_a, AIC_b, AIC_c)
model_type<- c("full model", "main effects", "intercept only")
delta_a<- AIC_a - AIC_a
delta_b<- AIC_a - AIC_b
delta_c<- AIC_a - AIC_c
K_vals<- c(3, 2, 1)
delta<- c(delta_a, delta_b, delta_c)
#now put all the AIC values in a df
AIC_table<- data.frame(
  model= model_type,
  K= K_vals,
  aic= AIC_vals,
  delta_AIC= delta
)
print(AIC_table)

################# part b ##########################################

# The full model is the only reasonable choice here. It has the lowest AIC and
# the main effects and intercept only models differ by 11.9 and 35.1, respectively.
# These differences are both above the "essentially no support" category. 
# Ecologically, this means that there is substantial support to group by watershed 
# type (urban vs. agricultural) when determining the relationship between DOM and absorbance.
# The relationship is significantly different in different watershed types.
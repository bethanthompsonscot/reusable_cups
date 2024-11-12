
###########################################################
###############      Latent class Nested logit  ###########
###########################################################



# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="lc-3choice-2classes-treatment-Q1x7-Q3x7-Q3x3-Q3x9_10_socio",
  modelDescr ="Latent class three choices two classes treatment plus Q1x7 Q3x7 Q3x3 Q3x9_10 socio",
  indivID   ="record", 
  nCores    = 3,
  seed      = 123456,
  outputDirectory = "data/mnl_output/"
)

# Define estimation settings
estimate_settings =list(
  maxIterations =500) 

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.table("data/apollo/short_conjoint_all_df.csv",header=TRUE, dec=".",sep=",")

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(DIS=1, RET=2, OUT=4),
  avail        = 1,
  choiceVar    = database$CHOICE,
  explanators  = database[, c("hGroup", "Q1x7", "Q3x7", "Q3x3", "Q3x9_10", "Q1x2", "Q1x1")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta =  c(
  m1_DIS       =   0,
  m1_RET       =   0,
  m1_OUT       =   0,
  m1_CHA_DIS   =   0,
  m1_DSC_RET   =   0,
  m1_DAM_RET   =   0,
  m1_DIS_RET   =   0,
  m2_DIS       =   0,
  m2_RET       =   0,
  m2_OUT       =   0,
  m2_CHA_DIS   =   0,
  m2_DSC_RET   =   0,
  m2_DAM_RET   =   0,
  m2_DIS_RET   =   0,
  #################
  m1_delt  =  0,
  m2_delt  =  0,
  m1_hGroup = 0,
  m2_hGroup = 0,
  m1_Q1x7 = 0,
  m2_Q1x7 = 0,
  m1_Q3x7 = 0,
  m2_Q3x7 = 0,
  m1_Q3x3 = 0,
  m2_Q3x3 = 0,
  m1_Q3x9_10 = 0,
  m2_Q3x9_10 = 0,
  m1_Q1x2 = 0,
  m2_Q1x2 = 0,
  m1_Q1x1 = 0,
  m2_Q1x1 = 0) 


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("m1_DIS","m2_DIS","m1_delt", "m1_hGroup", "m1_Q1x7", "m1_Q3x7", "m1_Q3x3", "m1_Q3x9_10", "m1_Q1x2", "m1_Q1x1")
# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["m_DIS"]] = list(m1_DIS, m2_DIS)
  lcpars[["m_RET"]] = list(m1_RET, m2_RET)
  lcpars[["m_OUT"]] = list(m1_OUT, m2_OUT)
  lcpars[["m_CHA_DIS"]] = list(m1_CHA_DIS, m2_CHA_DIS)
  lcpars[["m_DSC_RET"]] = list(m1_DSC_RET, m2_DSC_RET)
  lcpars[["m_DAM_RET"]] = list(m1_DAM_RET, m2_DAM_RET)
  lcpars[["m_DIS_RET"]] = list(m1_DIS_RET, m2_DIS_RET)
  
  V=list()
  V[["class_1"]] = m1_delt + m1_hGroup * hGroup + m1_Q1x7 * Q1x7 + m1_Q3x7 * Q3x7 + m1_Q3x3 * Q3x3 + m1_Q3x9_10 * Q3x9_10 + m1_Q1x2 * Q1x2 + m1_Q1x1 * Q1x1
  V[["class_2"]] = m2_delt + m2_hGroup * hGroup + m2_Q1x7 * Q1x7 + m2_Q3x7 * Q3x7 + m2_Q3x3 * Q3x3 + m2_Q3x9_10 * Q3x9_10 + m2_Q1x2 * Q1x2 + m2_Q1x1 * Q1x1
  
  classAlloc_settings = list(
    classes      = c(class_1=1, class_2=2), 
    utilities    = V
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives  = c(DIS=1, RET=2, OUT=4),
    avail         = 1,
    choiceVar     = CHOICE
  )
  
  ### Loop over classes
  s=1
  while(s<=2){
    
    
    V = list()
    V[['DIS']]        =  m_DIS[[s]] + m_CHA_DIS[[s]] * CHA_DIS
    V[['RET']]        =  m_RET[[s]] + m_DIS_RET[[s]] * DIS_RET + m_DSC_RET[[s]] * DSC_RET + m_DAM_RET[[s]] * DAM_RET
    V[['OUT']]        =  m_OUT[[s]]    
    

    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### CLASSICAL ESTIMATION FOR COVARIANCE MATRIX                  ####
# ################################################################# #

### Reinstate original vector of fixed parameters


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik",maxIterations =500,estimationRoutine="BFGS"))
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model,
                   modelOutput_settings=list(printPVal=TRUE))

model$estimate
model$BIC
model$AIC

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model,
                  saveOutput_settings=list(printPVal=TRUE,saveCov=TRUE))













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
  modelName ="mnl_4",
  modelDescr ="mnl_4_cup_option",
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

database = read.table("data/apollo/full_conjoint_all_df.csv",header=TRUE, dec=".",sep=",")

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(DIS=1, RET=2, REF=3, OUT=4),
  avail        = 1,
  choiceVar    = database$CHOICE,
  explanators  = database[,]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(                   
  b_DIS   =   0, 
  b_RET   =   0,   
  b_REF   =   0,   
  b_OUT   =   0,   
  b_CHA_DIS   =   0,   
  b_DSC_RET   =   0,   
  b_DAM_RET   =   0,   
  b_DIS_RET   =   0,   
  b_DIS_REF   =   0)   


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b_DIS")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  
  V = list()
  V[['DIS']]        =  b_DIS + b_CHA_DIS * CHA_DIS
  V[['RET']]        =  b_RET + b_DIS_RET * DIS_RET + b_DSC_RET * DSC_RET + b_DAM_RET * DAM_RET
  V[['REF']]        =  b_REF + b_DIS_REF * DIS_REF
  V[['OUT']]        =  b_OUT 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(DIS=1, RET=2, REF=3, OUT=4),
    avail         = 1,
    choiceVar     = CHOICE,
    V             = V
  )
  
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
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

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model,
                  saveOutput_settings=list(printPVal=TRUE,saveCov=TRUE))


# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Optional: load previously estimated model object
#model = apollo_loadModel(apollo_control$modelName)

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)
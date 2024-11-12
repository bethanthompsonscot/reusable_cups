# README

### What is this repository for?

-   This repository should enable you to reproduce the results in the paper **Changing consumer preferences for single-use disposable and reusable cups: regulation, persuasion and motivation - doi.**

### How do I get set up?

-   Data to replicate this project is available upon application to ReShare via the UK Data Service. Once the data is obtained the raw data and design file should be placed in the /data folder to enable the file paths to work as written. You will also need to maintain the file structure for the same reason.
-   First install the targets and tarchetypes packages. Then run the targets workflow from the \_targets.R script. If you do not have the packages from the tar_option_set installed, you will also need those. Once the target workflow is complete you are ready to conduct the analysis as the tidied and cleaned data will now be ready as .csv files in the data/apollo folder.
-   From the scripts/analysis/apollo folder you can run the various discrete choice models from the paper. They will write the results to the data/mnl_output folder.
-   Once the discrete choice models have run, you can go to the scripts/paper_tables folder where the scripts extract the model results and provide the code for the secondary analysis.
-   The scripts in the simulation_b_OUT_constant folder re-run the conditional logit models for the 3 and 4 cup options but with b_OUT held constant instead of b_DIS. This is more useful for modelling purposes. These are used to generate Figure 1.
-   The analysis for the paper was conducted in R version 4.3.1.

### Who do I talk to?

-   Any questions you can contact me: bethan.thompson\@sruc.ac.uk

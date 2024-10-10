### Scripts for "Empirical Validation of Genomic Offset to Environmental Change in an Intensively Monitored Songbird Population"

Authors: Katherine Carbeck, Peter Arcese, Jen Walsh

**Abstract**
The genomic basis of local adaptation is a key focus of evolutionary and conservation biologists aiming to predict species’ responses to rapid environmental change. Because the mechanisms promoting local adaptation among subdivided populations can be complex, forecasting the causal links between individual genotype, phenotype, environment, and fitness represents an essential first step in developing conservation policy and testing evolutionary theory. Here, we employed space-for-time substitution to test if genomic mismatch to local environment predicted individual survival, reproductive success or population growth rate. We leveraged 48 years of demographic information from an insular song sparrow (Melospiza melodia) population resident on X̱OX̱ DEȽ/Mandarte Island, British Columbia, Canada, which has fluctuated dramatically in response to variation in local climate. We then compared demographic performance in our focal population to whole genome data derived from song sparrows across the species’ continent-wide range to model genotype-environment interactions at the continental scale and test model predictions at the population scale. Genomic offset varied greatly from 1975-2023, with notable peaks coincident with climate extremes that appeared to negatively affect juvenile survival and population growth rate two years later. Our findings suggest that genomic mismatches to the local environment resulted in lagged impacts on population growth that were closely linked to juvenile survival. Our results are consistent with the hypothesis that genomic state can predict demographic performance in local populations and suggest that long-term studies offer novel routes to test evolutionary theory and its application to conservation.

**Author contirbutions**
KC, PA, and JW conceived and designed the study. JW conducted the laboratory work. KC performed bioinformatic analyses, modeling work, and analyzed output with input from all co-authors. JW and KC wrote the manuscript with substantial input from PA. 

#### Workflow

1. For whole genome processing and bioinformatic pipelines [click here](https://github.com/kcarbeck/SOSP-WGS/tree/main/bioinformatics-pipeline)

2. Create climate raster stacks using ClimateNA and output text file for gradient forest ([01.createClimateRasters.R](01.createClimateRasters.R)). Then, create envfile for gradient forest ([02.populationLevelEnvData.py](02.populationLevelEnvData.py))

3. Train gradient forest model using [helper script](03.GF_TrainingHelperScript.py) and [training script](03.gradient_training.R). For selection of SNPs [click here](https://github.com/kcarbeck/SOSP-WGS/tree/main/WZA).

4. Predict GF model for each year using [helper script](04.GF_PredictionHelperScript.py) and [prediction script](04.gradient_prediction_script.R).
   
5. Calculate genomic offset between each consecutive year using [helper script](05.GF_OffsetHelperScript.py) and [gradient offset script](05.gradient_offset_script.R).
   
6. Extract genomic offset output for each year for Mandarte ([06.extractOffset.R](06.extractOffset.R)).

#### Analyses

1. Performed a PCA on climate variables used in GF for Mandarte and visualized variation over time and in a bioplot. Script: [07.0.climatePCA.R](07.0.climatePCA.R)
2. Visualized annual variation in demmographic variables, genomic offset, and climate using [07.plotOffsetDemographyRelationships.R](07.plotOffsetDemographyRelationships.R).
3. Evaluated the impact of genomic offset on demography using cross-correlation function (CCF) analyses and generalized least squares (GLS) models and visualized results. Scripts: [08.timeSeriesScatterPlots.R](08.timeSeriesScatterPlots.R) and [09.GLSmodel.R](09.GLSmodel.R)
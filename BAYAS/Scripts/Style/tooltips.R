# Contains each tooltip that are used

tooltip <- list(
  # General
  clickForInformation = "Click for information!",

  
  # Upload page
  dataUpload = "Upload your data in a single csv file.",
  dataInputFormat = "The input data should be in long-format.", 
  dataProperties = paste0("<p>The software has guessed properties of the variables from the csv file. Please verify the properties of each variable!</p>",
                          "<p>These inputs are important! The affect what models you can select on the next page.</p>",
                          "<p>For further information on the three properties, hover over the head of the respective column.</p>"),
  dataPropertiesCharacteristic = paste0("<p>The type of data per variable (column).</p>",
                                        "<p>Use \"Discrete\" for count variables, i.e. whole numbers. Examples are cell numbers, animal numbers, etc.</p>",
                                        "<p>Use \"Continuous\" for floating point numbers, e.g. size in mm or weight in kg.</p>",
                                        "<p>Use \"Categorical\" for named data, e.g. sex (female/male), genotype (wt/mut1/mut2) etc.</p>"),
  dataPropertiesLowerLimit = paste0("<p>The theoretically possible smallest value.</p>",
                                        "<p>If the data are probabilities and you have only values greater than 0 till 1, still use 0 as a lower limit, as it is in theory possible. </p>",
                                        "<p>If negative values can occur, choose \"-INF\" (negative infinity) as a lower limit.</p>",
                                        "<p>For some data, e.g. logarithm, use \">0\", that means your lower limit is greater than 0, by excluding 0. </p>"),
  dataPropertiesUpperLimit = paste0("<p>The theoretically possible greatest value.</p>",
                                    "<p>If the data are probabilities and you have only values between 0 and smaller than 1, use 1 as a upper limit, as it is in theory possible. </p>",
                                    "<p>If the data are positive and greater than 1, use \"INF\" (infinity) as upper limit.</p>"),
  responseVariable = paste0("<p>The response variable or dependent variable is the outcome variable you are interested in.</p>",
                            "<p>E.g. if you want to analyse the <b>size</b> of mice depending on <b>weight</b> and <b>age</b>, your response variable is <b>size</b>.</p>"),
  plotType = "Some plot types are only available for the first variable, e.g. histogram or density.",
  
  
  # Model selection page
  useOfVariableCheckbox = paste0("<p>Deselect a variable that you are not interested in.</p>",
                                 "<p>(De)selection has an effect to the applicable models!</p>"),
  selectionOfTerms = paste0("<p>Choose for each predictor your variable. Read the description to understand the role of this variable in the model.</p>",
                            "<p>Depending on the model you can <b>Add Term(s)</b>. Choose a single or a multiplication term, to add a additional predictor.</p>"),
  howToRemovePredictor = paste0("<p>Click on a predictor in this line to remove it.</p>"),
  samePriorDistribution = paste0("<p>This model uses the same distribution function for all predictors.</p>"),
  recommendedValues = paste0("<p>The prior scales will be adjusted by the data, to obtain as reliable as possible weakly informative priors.</p>"),
  
  # Run fit page
  iteration = paste0("<p>The number of iterations is the number of samples drawn from the posterior. By default, half of them will be used for warmup, the other half will be used to evaluate the posterior.</p>",
                     "<p>The samples have to be large enough to be representative of the posterior.</p>",
                     "<p>The default number of iterations is 2000. </p>"),
  # iteration = paste0("<p>tesst"), 
  chain = paste0("<p>Also Markov chains. The number of Markov chains produced.</p>",
                 "<p>We need several Markov chains to assess whether the posterior has been sampled sufficiently.</p>",
                 "<p>Default are 4 Markov chains. </p>"), 
  cores = paste0("<p>Set the number of processor cores.</p>",
                 "<p>The more cores, the faster the sampling. However, do not use more cores than your machine has.</p>"), 
  adaptDelta = paste0("<p>The proposal acceptance probability. A higher value forces Stan to take smaller steps.</p>",
                      "<p>This increases the accuracy but slows the sampling.</p>",
                      "<p>Default is 0.8. </p>"),
  maxTreepdeth = paste0("<p>An efficiency parameter, that sets the maximum tree depth of stans sampler. A higher value could slow the sampling.</p>",
                      "<p>Default is 10. </p>"),
  setSeed = paste0("<p>The seed used for sampling to make it reproducible.</p>",
                        "<p>If not used, a random seed is used and can be reviewed in the report.</p>"),
  
  removeCPIDM = paste0("<p>Remove the current selected fitted model. To reduce memory; just in case you will download the session.</p>"),
  
  
  checklistGeneralInformation = paste0(
    tags$div(tags$p("Some worthwhile information about the data and model."),
             tags$ul(
               tags$li(
                 "Missing values: Your data contains missing values. Only complete cases are used for the fit."
               )
               )
             )),
  checklistESS = paste0("<div>",
                       "<p>Roughly speaking, the effective sample size (ESS) of a quantity of interest captures how many independent draws contain the same amount of information as the dependent sample obtained by the MCMC algorithm.</p>",
                       "<p>The ESS for each single parameter should be at least greater than 100 times number of chains. Bulk ESS are more severe than Tail ESS.</p>", 
                       "<p>If you have problems with ESS you could try the following:</p>",
                       "<ul>",
                       "<li>Increase the number of iterations. Default are 2000 iterations on 4 chains. E.g. double the iterations to 4000.</li>",
                       "<li>Revise your model e.g. select better priors.</li>",
                       "<li>Consider, if possible, another model.</li>",
                       "</ul></div>"),
  checklistRhat = paste0("<div>",
                         "<p>R-hat convergence diagnostic compares the between- and within-chain estimates for model parameters and other univariate quantities of interest.</p>",
                         "<p>A R-hat value larger than 1 indicate problems of mixing. It should be lesser than 1.01.</p>", 
                         "<p>If you have larger R-hat values try the following:</p>",
                         "<ul>",
                         "<li>Increase the number of iterations. Default are 2000 iterations on 4 chains. E.g. double the iterations to 4000.</li>",
                         "<li>Revise your model e.g. select better priors.</li>",
                         "<li>Consider, if possible, another model.</li>",
                         "</ul></div>"), 
  checklistDivTrans = paste0("<div>",
                         "<p>Divergent transitions occur, when there are problems exploring the target distriubtion. For hard problems the step size of the sampler (adpat_delta) is to large to get the best parameter.</p>",
                         "<p>Just a few divergent transition are not that serious</p>", 
                         "<p>If you got more of them it turns serious and you have to try the following:</p>",
                         "<ul>",
                         "<li>Increase the sample parameter \"Adapt delta\" under \"Advanced Settings\" from default 0.8 to e.g. 0.9. Note: A larger adapt_delta value also leads to a longer computational time.</li>",
                         "<li>Revise your model e.g. select better priors.</li>",
                         "<li>Consider, if possible, another model.</li>",
                         "</ul></div>"),
  checklistTreeDepth = paste0("<div>",
                             "<p>Exceeding the maximum treedepth is a efficiency concern, but should also be handled.</p>",
                             "<p>If there are just a few warnings about exceeding the maximum treedepth it is not that serious.</p>",
                             "<p>Try the following to avoid this warning:</p>",
                             "<ul>",
                             "<li>Increase the sample parameter \"Max treedepth\" under \"Advanced Settings\" stepwise from default 10 to e.g. 12 or 15. Note: A larger Max treedepth value also leads to a much longer computational time!</li>",
                             "</ul></div>"),
  checklistPPC = paste0("<div>",
                        "<p>Posterior predictive checks (PPC) are used to compare simulated data based on the fitted model to the observed (your) data. If the replicate curves matches the thick one, the model is able to reproduce the data.</p>",
                        "<p>Use the PPC (upper right) to look for systematic discrepancies between your data and the replicas. Bigger discrepancies will also highlighted with orange to red overlays.</p>",
                        "<p>(Discrepancies are calculated using the z-score between the predictions and the actual data. Z-scores above 1.5 trigger warnings, while those above 2 indicate errors.)</p>",
                        "<p>Systematic discrepancies occur mostly due to: </p>",
                        "<ul>",
                        "<li>Lack of information: Consider, if possible, another predictor.</li>",
                        "<li>Wrong model: Consider, if possible, another model.</li>",
                        "<li>Too informative prior: Have a look on prior_vs_posterior check (Model validation tab) and adjust your priors.</li>",
                        "</ul>",
                        "<p><b>Note: Even with (slight) discrepancies in the PPC, that model could be still the \"best\"!</b></p>",
                        "</div>"),
  checklistQuantities = paste0("<div>",
                        "<p>The effect probability (pi) is the larger half side fraction of a parameters distribution. With a pi of 1, the distribution is one sided and as certain as possible. Where a pi of 0.5 is totally uncertain. </p>",
                        "<p>Have a look at the table on the bottom right \"Effects\" which summarize the quantities of each single parameter with the effect probability pi.</p>",
                        "<p>The graph on the bottom left shows also the quantitites (marginal posteriors) of each parameter. The plot is also available in more detail on the separated tab \"Marginal posteriors\".</p>",
                        "</div>"), 
  
  mpSummaryTablePIValue = paste0("<p>The threshold agains the pi-value is calculated. Default is 0.</p>"),
  
  labelHDI = paste0("<p>The probability mass to include in the highest density interval.</p>"),
  labelInnerHDI = paste0("<p>The probability mass to include in the inner interval.</p>"),
  labelOuterHDI = paste0("<p>The probability mass to include in the outer interval.</p>"),
  
  downloadMP = paste0("<p>Download the current plot.</p>"),
  
  closeCurrentMP = paste0("<p>Remove the current plot.</p>"),
  
  
  # Model prediction
  modelPredictionGroupEffects = paste0("<p>Select <b>single</b> group variables to compare effects within this group.</p>",
                                       "<p>Select <b>several</b> group variables (by holding CTRL) to compare effects within the subgroups</p>"),
  
  modelPredictionSlopeEffects = paste0("<p>Select <b>single</b> slope variables to show the slope effect itself. If the model contains group dependent slope, they will be shown grouped.</p>",
                                       "<p>Select <b>several</b> slope variables (by holding CTRL) to show the interaction slope (if the model contains such an interaction).</p>"),
  
  modelPredictionNumInputs = paste0("<p>This is a group dependent variable. This implies that the group effect may depends on the actual value of this numeric. Use proper values to examine the effect.</p>"),
  
  modelPredictionPlotType = paste0("<p>The shown plot type. Density plots are useful to examine the overlapping area, whereas violin plots are more clear.</p>"),
  modelPredictionFullPosterior = paste0("<p>Using the \"Full\" posterior means to include also the global variance. Small effects related to the global variance  may be negligible. Therefore it is also good to examine effects including the global variance.</p>"),
  modelPredictionCI = paste0("<p>The Highest Density Interval (HDI) maximizes the density of a certain CI, whereas the Equal-Tailed Interval (ETI) assigns equal density to both tails. For symmetric and steady distributions both methods act identical.</p>"),
  modelPredictionCIValue = paste0("<p>Mostly used are CI of 0.9, 0.89 and 0.95.</p>"),
  
  modelPredictionOverlapGlobal = paste0("<p>It is not possible to show a single distribution of the effect when including the global variance.</p>",
                                        "<p>The plot shows the distribution of overlapping values of each effect distribution pair. A value of 0 means a clear effects, while 1 means no effects.</p>",
                                        "<p>If a clear effect is desired, the distribution should be around 0.</p>"),
  
  modelPredictionDifference = paste0("<p>The plot shows the distribution of the difference between the two effect distributions. A PI-value of 1 indicates a clear difference between the predictions, while a PI-value around 0 suggests no difference.</p>"),
  
  ### Planning
  planningExampleDataSet1 = paste0("<p>An empty dataset.</p>"),
  planningExampleDataSet2 = paste0("<p>A dataset of two columns of continuous data of linear dependency.</p>"),
  planningExampleDataSet3 = paste0("<p>A dataset of continuous data separated by 3 groups.</p>"),
  planningExampleDataSet4 = paste0("<p>A dataset with e continuous reponse and an empty predictor X.</p>"),
  planningExampleDataSet5 = paste0("<p>A dataset of three columns of continuous data of linear dependency.</p>"),
  
  planningSampleSizeUnit = paste0("<p>The data points (rows) for each element in the given categorical variable.</p>"),
  planningSampleSize = paste0("<p>The total number of datapoints (rows) in the dataset.</p>"),
  
  
  ### Report
  reportAllElements = paste0("<p>Reports all items in given order from the above list.</p>"),
  reportRecommendedOrder = paste0("<p>Sort the items to a recommended order. </p>"),
  reportSingleElements = paste0("<p>Reports only the selected item from the above list.</p>")
  
)

initTooltips <- function(language = "en"){
  if(language == "en"){
    tooltip <<- tooltip_en
  }else{
    
  }
}

tooltip_en <- list(
  
)
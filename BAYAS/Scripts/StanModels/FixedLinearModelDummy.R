
# Extend the list of characterstic with 'VAR' for variable/changeable
cEnum <- characteristicEnum()
cEnum$VAR <- 'VAR'


# Fixed Linear Model with intercept + norm Term + Dummy var
# cont ~ Var + Int
FixedLinearModelDummyStanModel <- R6Class(classname = "GLMBinomialStanModel",
                                          inherit = BAYSISAbstractModel,
                                          
                                          
                                          public = list(
                                            id = NULL, #new id!
                                            display_name = 'Fixed linear model - Normal Distribution with intercept + var + Dummy',
                                            description = 'TEMP TEMP TEMP',
                                            
                                            number_co_var = 2,
                                            number_optional_co_var = 0,
                                            
                                            plot = NULL,
                                            
                                            plot_me = function(){
                                              seq <- seq(from = -10, to = 10, length = 100)
                                              val <- dnorm(x=seq,mean = 0, sd = 2)
                                              self$plot <- plot(val ~ seq, main = "Normal distribution", type = "l")
                                              self$plot
                                            },
                                            
                                            # Matrix of:
                                            # Columns: characteristic, lower limit, uper limit
                                            # Rows: Predictor, and each other
                                            parameters = data.frame(stringsAsFactors = F,
                                                                    characteristics = c(cEnum$Continuous, cEnum$VAR, cEnum$Discrete),
                                                                    lower_limit = c('-INF', 'VAR', '0'),
                                                                    upper_limit = c('INF', 'VAR', 'INF'),
                                                                    description = c('Response', 'First term', 'Second term'),
                                                                    distribution = c('Normal','Normal','Normal')
                                            ),
                                            
                                            # This variable hold the optional parameters. This could be any type of variable with any constraint. 
                                            # If there are no constraint, use 'VAR'.
                                            # The number of Variable (numberVar) are used to take the number of selectInputs.
                                            # The name of the data frames will show in the selectInput, when adding a new term.
                                            # PossibleAmount hold the number of times this kind of term can be added. Use 'VAR' if there are no limit.
                                            optional_values = list(
                                              
                                            ),
                                            
                                            # Returns a vector of named parameters
                                            get_open_values = function(){
                                              return('VAR')
                                            },
                                            
                                            # Run the stan code with given parameters
                                            run_stan_code = function(response, var, opt_var = NULL, data, stanParameter){
                                              formula <- paste0(response, " ~ ", var$var_name[1], " + ", var$var_name[2])
                                              print(formula)
                                              
                                              stan_result <- rstanarm::stan_glm(formula = formula, data=data, verbose = T, show_messages = T,
                                                                                iter = stanParameter$iterations, adapt_delta = stanParameter$adapt_delta, cores = stanParameter$cores,
                                                                                chains = stanParameter$chains)
                                              return(stan_result)
                                            }
                                          ),
                                          
                                          
                                          private = list(
                                            
                                          )
                                          
)
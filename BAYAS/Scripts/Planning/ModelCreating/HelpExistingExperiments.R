getExistingExperimentName <- function(i){
  case_when(
    i == 1 ~ "",
    i == 2 ~ "Two_Groups_comparison",
    i == 3 ~ "Simple_regression"
  )
}

getExistingExperimentInfo <- function(i){
  case_when(
    i == 1 ~ "An empty experiment. No defined variables or model.",
    i == 2 ~ "A typical 't-test' data set - two groups assumed to be normally distributed.",
    i == 3 ~ "A response variable that depends on a continuous measurement with normal noise."
  )
}



### Create preview plots ###
# theme_set(theme_bw())
#t-test

df <- data.frame(group=rep(c("control", "treatment"), each=1e6),
                 val = c(rnorm(1e6,2,1), rnorm(1e6, 3,1)))
ggplot(df) + 
  geom_density(aes(x=val, color=group), linewidth=1.5) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") 


#simple regression
lin_func <- function(x) 2.5+0.05*x
df <- data.frame(x = 1:50,
                 val = rnorm(50,lin_func(1:50),0.2))
ggplot(df, aes(x=x, y=val)) + 
  geom_point() +
  geom_smooth(method='lm', se=F) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")







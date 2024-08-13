# script for Factor Analysis (EFA and CFA)
  # author: Molly Grant 
  # date: 12 August 2024 
  # R version 4.4.1 (2024-06-14 ucrt)

########################
# overview ###
########################
# This R script provides simple code for both EFA and CFA ###
# The code for EFA uses the psych package and CFA uses lavaan ###
# The simple code is then followed with an example using a synthetic dataset labelled the efa_data dataset ###
########################

########################
# set-up ###
########################
#### load packages ####
packages <- c("dplyr", "psych", "factoextra", "lavaan","GPArotation", "ggplot2", "corrplot", "semPlot")

# Load all packages in the list
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

########################
# simple code for efa ###
########################

######### start #########
describe(data) #look at the variables.
dim(data) #retrieve the dimension of the data

sum(is.na(data)) #check for missing
boxplot(data) #check for outliers

######### correlations #########
datamatrix <- cor(data)
corrplot(datamatrix, method = "number") 

######### factorability #########
#Before we do factor analysis, we need to determine if the data are suitable for FA. We do this with the KMO and Bartlett Test of Sphericity.
  #Kaiser-Meyer-Olkin (KMO)
KMO(data) #The KMO measures the sampling adequacy. A high KMO value (closer to 1) indicates the data have sufficient correlations for factors to be extracted. Desired value >.5 (although higher than this is better). 

  #Bartlett test
cortest.bartlett(data[,2:6]) #similar to above, indicates if data are appropriate for FA. A p-value <.05 indicates appropriate. Note: can also run this on the correlation matrix

######### efa #########
factnumber <- psych::fa.parallel(data[,2:6], fa = 'fa', fm = 'ml', n.obs = 6368) #fa = PCA or FA; fm = what factor method to use
print(factnumber)

Loadings <- fa(data[,2:6], nfactors = 2, rotate = 'oblimin', fm = 'ml') # direct oblimin chosen so factors can be correlated if >1 factors
print(Loadings$loadings,cutoff = 0.3)

  # visual representation 
fa.diagram(Loadings, digits =  3) 

#run likelihood ration test to see how well the model fits the data
chi_square_value <- Loadings$STATISTIC
degrees_of_freedom <- Loadings$dof
p_value <- Loadings$PVAL

# Display the results
cat("Chi-square value:", chi_square_value, "\n")
cat("Degrees of freedom:", degrees_of_freedom, "\n")
cat("P-value:", p_value, "\n") #Indicates if the model fits the data. A non-significant p-value (e.g., > 0.05) suggests that the model fits the data well.

#The next step is to meaningfully interpret the factors which should be guided by the literature.

########################
# efa example ###
########################
#generate synthetic efa example dataset #########
set.seed(123)  # For reproducibility

# Define the number of observations
n <- 2000  

# Define the latent variables (factors)
Factor1 <- rnorm(n, mean = 0, sd = 1)
Factor2 <- rnorm(n, mean = 0, sd = 1)
Factor3 <- rnorm(n, mean = 0, sd = 1)

# Create observed variables with strong but not overly correlated loadings
x1 <- 0.7 * Factor1 + rnorm(n, 0, 0.4)
x2 <- 0.6 * Factor1 + rnorm(n, 0, 0.4)
x3 <- 0.65 * Factor1 + rnorm(n, 0, 0.4)
x4 <- 0.7 * Factor1 + rnorm(n, 0, 0.4)

x5 <- 0.7 * Factor2 + rnorm(n, 0, 0.4)
x6 <- 0.65 * Factor2 + rnorm(n, 0, 0.4)
x7 <- 0.68 * Factor2 + rnorm(n, 0, 0.4)
x8 <- 0.7 * Factor2 + rnorm(n, 0, 0.4)

x9  <- 0.7 * Factor3 + rnorm(n, 0, 0.4)
x10 <- 0.65 * Factor3 + rnorm(n, 0, 0.4)
x11 <- 0.68 * Factor3 + rnorm(n, 0, 0.4)
x12 <- 0.7 * Factor3 + rnorm(n, 0, 0.4)

# Combine all variables into a data frame
efa_data <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
head(efa_data)

######### start #########
describe(efa_data) #look at the variables.
dim(efa_data) #retrieve the dimension of the data

sum(is.na(efa_data)) #check for missing
boxplot(efa_data) #check for outliers

######### correlations #########
datamatrix <- cor(efa_data)
corrplot(datamatrix, method = "number") #note: check for any major multi-collinearity issues. Correlation >.8 consider removing (although some multicollinearity is OK in FA)

######### factorability #########
#Before we do factor analysis, we need to determine if the data are suitable for FA. We do this with the KMO and Bartlett Test of Sphericity.
#Kaiser-Meyer-Olkin (KMO)
KMO(efa_data) #The KMO measures the sampling adequacy. A high KMO value (closer to 1) indicates the data have sufficient correlations for factors to be extracted. Desired value >.6

#Bartlett test
cortest.bartlett(efa_data) #similar to above, indicates if data are appropriate for FA. A p-value <.05 indicates appropriate. Note: can also run this on the correlation matrix

######### efa #########
#To decide how many factors to extract, we need to look at the scree plot and eigenvalues.First, we look at the data without an extraction or rotation.
fafit <- psych::fa(efa_data,nfactors = ncol(efa_data), rotate = "none")
n_factors <- length(fafit$e.values) #Extract the eigenvalues before any factor extraction or rotation has been applied.
scree <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafit$e.values) #scree plot
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix before factor analysis)") #This displays the scree plot (looks a little different on synthetic data)

#Now we run the factor analysis
factnumber <- psych::fa.parallel(efa_data, fa = 'fa', fm = 'ml', n.obs = 2000) #fa = PCA or FA; fm = what factor method to use.
print(factnumber) #This gives the eigenvalues and scree plot which we can use to choose the number of factors (eigenvalues >1)

Loadings <- fa(efa_data, nfactors = 3, rotate = 'oblimin', fm = 'ml') # direct oblimin chosen so factors can be correlated if >1 factors
print(Loadings$loadings, cutoff = 0.3) #Use threshold of >.3
#There are a lot of cross loading items, so we could try three factors.

# visual representation 
fa.diagram(Loadings, digits = 2, main = "Factor Analysis Diagram", cut = 0.3, simple = T)

#run likelihood ration test to see how well the model fits the data
chi_square_value <- Loadings$STATISTIC
degrees_of_freedom <- Loadings$dof
p_value <- Loadings$PVAL

# Display the results
cat("Chi-square value:", chi_square_value, "\n")
cat("Degrees of freedom:", degrees_of_freedom, "\n")
cat("P-value:", p_value, "\n") #Fits data well

########################
# simple code for cfa ###
########################

# Define the CFA model
model <- '
  Factor1 =~ x1 + x2 + x3
  Factor2 =~ x4 + x5 + x6
  Factor3 =~ x7 + x8 + x9
'

# Fit the model to the data
fit <- lavaan::cfa(model, data = data)

# Summarize the results with fit indices
summary(fit, fit.measures = TRUE, standardized = TRUE, rsq = TRUE)
#we can assess how well the model fits the data through these fit indices.
#we want to make sure all variables load >.3 onto each factor.
#we also want the TLI & CFI >.95; SRMR < .08;  RMSEA and 90% CIs below .05. 
#Chi-square values are commonly reported in SEM with a statistically significant value indicating poor fit however, this is highly sensitive to sample size


# Check modification indices if needed
modificationIndices(fit, sort = TRUE) #we can try to adjust the model by adding extra paths.
fitmeasures(fit, fit.measures = "all", baseline.model = NULL)
# Visualize the CFA model
semPaths(
  fit,                        # Your CFA/SEM model fit
  what = "std",               # Standardized estimates
  layout = "tree",            # Tree layout
  edge.color = "black",       # Color of the paths
  edge.label.cex = 1,         # Size of edge labels
  edge.width = 1.5,           # Thickness of the edges
  sizeMan = 8,                # Size of manifest (observed) variables
  sizeLat = 10,               # Size of latent variables
  shapeMan = "rect",          # Shape of manifest variables (rectangular)
  shapeLat = "ellipse",       # Shape of latent variables (elliptical)
  color = list(lat = "lightblue", man = "lightgreen", int = "white"),  # Custom colors
  nodeLabels = c("X1", "X2", "X3", "F1", "F2"),  # Custom labels for nodes (example)
  title = TRUE,               # Include title of the diagram
  mar = c(6, 6, 6, 6)         # Margins around the plot
)

#Extract factor scores
fitPredict <- as.data.frame(predict(fit)) # this will give you the factor scores. you will need to show that these are highly correlated with the sum scores if you prefer to use the sums scores. 
data_updated <- cbind(data, fitPredict) #join your data with your factor scores

########################
# cfa example ###
########################
######### cfa example dataset (using same dataset as above #########
# Define the CFA model
model <- '
  Factor1 =~ x12 + x9 + x10 + x11 
  Factor2 =~ x8 + x7 + x6 + x5 
  Factor3 =~ x1 + x4 + x3 + x2 
'

# Fit the model to the data
fit <- cfa(model, data = efa_data) 

# Summarize the results with fit indices
summary(fit, fit.measures = TRUE, standardized = TRUE)
#this model fits the data well, no need for adjustments 

# Check modification indices if needed
modificationIndices(fit) 

# Visualize the CFA model
semPaths(fit, "std", layout = "tree")

#Extract factor scores
fitPredict <- as.data.frame(predict(fit)) # this will give you the factor scores. 

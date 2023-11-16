
library(purrr)
df <- read.csv("data/data_understanding_plsv01.csv")

# Functions 


# population sd
sd.p <- function(x){sd(x)*sqrt((length(x)-1)/length(x))}


stand_vector <- function(x){
    vec_mean = mean(x)
    vec_sd = sd.p(x)
    stand_vec = (x-vec_mean) / vec_sd
    return(round(stand_vec,3))}

#population cov
cov.p <- function(x,y=NULL) {
  cov(x,y,)*(NROW(x)-1)/NROW(x)}

# Step 1 standardize each vector in Dataset
df <- purrr::map_df(df, stand_vector)

# Step 2 Initialize outer model weights

outer_weights = c(
                path_product_1 = 1,
                path_product_2 = 1,
                path_product_3 = 1,
                path_service_1 = 1,
                path_service_2 = 1,
                path_service_3 = 1,
                path_loyalty_1 = 1,
                path_loyalty_2 = 1
                )
# Keep a matrix that records each iteration of weights
outer_weights_matrix <- matrix(outer_weights, nrow = 1, ncol = length(outer_weights))


# PLSPM iterates until it converges on a set of weights for the manifest variables 
while(TRUE) {
  
# Step 3 Calculate Latent Variable Scores
# A latent variable score is the weighted sum of the standardized manifest variables in a block.
product_latent_score <- c()
service_latent_score<- c()
loyalty_latent_score <- c()
#product_latent_score
for(i in 1:nrow(df)){product_latent_score[i] = (sum(df[i,1:3] * outer_weights[1:3]))}
#service
for(i in 1:nrow(df)){service_latent_score[i] = (sum(df[i,4:6] * outer_weights[4:6]))} 
#loyalty_latent_score 
for(i in 1:nrow(df)){loyalty_latent_score[i] = (sum(df[i,7:8] * outer_weights[7:8]))} 

# Step 3a standardize the weighted latent variable scores  
product_latent_score <- stand_vector(product_latent_score)
service_latent_score <- stand_vector(service_latent_score)
loyalty_latent_score <- stand_vector(loyalty_latent_score)

# Step 4 Calculate Inner Path Coefficients !!!!! Maybe wrong wording here
# by taking the covariance between exogenous and endogenous latent scores 
####Rename#####
product_loyalty_path <- cov.p(product_latent_score,loyalty_latent_score)
service_loyalty_path <- cov.p(service_latent_score,loyalty_latent_score)

# Step 5 Calculate Inner Estimate/ Proxy & standardize 
# by multiplying the path values/ coefficients by the endogenous latent scores
# then standardize the vector
inner_product <- stand_vector(product_loyalty_path * loyalty_latent_score)
inner_service <- stand_vector(service_loyalty_path * loyalty_latent_score)
inner_loyalty <-stand_vector(product_loyalty_path*product_latent_score + service_loyalty_path*service_latent_score)

# Step 6 update outer model weights 
outer_weights = c(
  path_product_1 = cov.p(df$Product1,inner_product),
  path_product_2 = cov.p(df$Product2,inner_product),
  path_product_3 = cov.p(df$Product3,inner_product),
  path_service_1 = cov.p(df$Service1,inner_service),
  path_service_2 = cov.p(df$Service2,inner_service),
  path_service_3 = cov.p(df$Service3,inner_service),
  path_loyalty_1 = cov.p(df$Loyalty1,inner_loyalty),
  path_loyalty_2 = cov.p(df$Loyalty2,inner_loyalty)
)

# Step 6a standardize the path outer weights 
# calculate Auxiliary Model to standardize weights
# not sure why this is done
wm <- matrix()
wm <- matrix(rep(0,ncol(df)), nrow = nrow(df), ncol = ncol(df))
for(i in 1:ncol(df)){wm[,i]<- df[[i]] *outer_weights[i]}
product_wm <- sd.p(rowSums(wm[,1:3]))
service_wm <- sd.p(rowSums(wm[,4:6]))
loyalty_wm <- sd.p(rowSums(wm[,7:8]))
outer_weights[1:3] <- outer_weights[1:3]/ product_wm
outer_weights[4:6] <- outer_weights[4:6]/ service_wm
outer_weights[7:8] <- outer_weights[7:8]/ loyalty_wm

# add weights for record keeping (only used for understanding algorithm) 
outer_weights_matrix <- rbind(outer_weights_matrix, outer_weights)

# Step 7 Check for convergence of weights
prior_weights <- nrow(outer_weights_matrix)-1
stop_value <- sum(abs(as.vector(outer_weights_matrix[prior_weights,]) - outer_weights))
if(stop_value < 0.0000001){break}
}

rownames(outer_weights_matrix) <- NULL


# Step 7 calculate the outer loading (Outer Model)
outer_loadings <- c(
 purrr::map_dbl(df[,1:3] ,cov.p, product_latent_score),
 purrr::map_dbl(df[,4:6], cov.p, service_latent_score),
 purrr::map_dbl(df[,7:8], cov.p, loyalty_latent_score)
)

# step 8 calculate the regression coefficients between latent variables (Iner Model)
df_latent_scores <-  data.frame('loyalty_latent_score' = loyalty_latent_score, 'product_latent_score' = product_latent_score, 'service_latent_score' = service_latent_score)
ols_model <- lm(loyalty_latent_score ~ product_latent_score +service_latent_score, df_latent_scores)
summary <- summary(ols_model)
r2 <- summary$r.squared
product_loyalty_path <- summary$coefficients[2,1]
service_loyalty_path <- summary$coefficients[3,1]

if (interactive()) {
print("Values for Manifest Variable Loadings:")
print(outer_loadings)

print("Regression coefficients between latent variables")
print(paste("Product to loyalty path coefficient",product_loyalty_path))
print(paste("Service to loyalty path coefficient",service_loyalty_path))
print(paste("Model R2:", r2))
}



############################################
#
# Script for generating synthetic human height data.
#
############################################

require( ggplot2 );

# Means and SDs from https://ourworldindata.org/human-height
male_mean <- 178.4; male_sd <- 7.6
female_mean <- 164.7; female_sd <- 7.1

# Generate synthetic male and female height samples with given means and SDs.
n <- 1000;
male_heights <- rnorm(n, mean = male_mean, sd = male_sd );
female_heights <- rnorm(n, mean = female_mean, sd = female_sd );

# Put them into a data frame together for cleanliness.
heights <- c( male_heights, female_heights );
sex <- c( rep('M',n), rep('F',n) )
df_heights <- data.frame( 'sex'=sex, 'height'=heights );

# Plot a histogram for each of the male and female heights
pp <- ggplot( df_heights, aes(x=height, color=sex, fill=sex ) );  
pp <- pp + geom_histogram( position="identity", alpha=0.5)

# The same data, but not splitting out bysex.
pp <- ggplot( df_heights, aes(x=heights ) );
pp <- pp + geom_histogram(fill="grey", color="black")

# Set up variables needed to fit and plot a normal
min_height <- min( heights );
max_height <- max( heights );
xmin <- 10*floor( min_height/10 )
xmax <- 10*ceiling( max_height/10 )
x <- seq( xmin, xmax, length.out=length(heights) );
y <- dnorm( x, mean(heights), sd(heights) )
df_normal <- data.frame('x'=x, 'y'=y);

# Estimate the male and female means separately and evaluate normal pdf
x <- seq( xmin, xmax, length.out=length(heights) )
normals_male <- dnorm(x, mean( male_heights), sd(male_heights) );
normals_female <- dnorm(x, mean( female_heights), sd(female_heights) )
sex <- c( rep('M',length(normals_male)), rep('F',length(normals_female)));
df_bimodal <- data.frame('x'=c(x,x), 'sex'=sex, 'density'=c(normals_male,normals_female) );



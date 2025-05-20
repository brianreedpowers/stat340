nSamp <- 1000
df <- data.frame(i=1:nSamp,
                 x=rnorm(nSamp,0,1))
df$px <- pnorm(df$x)



#library(animate)
#device <- animate$new(600, 400)
#attach(device)  # overrides the 'base' primitives

setwd("output")
for(nTo in 1:200){
  png(sprintf("img_%03d.png", nTo))
  par(mfrow=c(2,2))
  par(mar=c(0,0,1,1))
  #pdf
  plot(seq(-4,4,.1),dnorm(seq(-4,4,.1)), type="l", xaxt="n", yaxt="n")
  segments(x0=df$x[1:nTo], y0=0, y1=dnorm(df$x[1:nTo]))
  segments(x0=df$x[nTo], y0=0, y1=dnorm(df$x[nTo]), col="red", lwd=2)
  #cdf
  plot(seq(-4,4,.1),pnorm(seq(-4,4,.1)), type="l", xaxt="n", yaxt="n")
  segments(x0=df$x[1:nTo], y0=0, y1=pnorm(df$x[1:nTo]))
  segments(x0=-4,x1=df$x[1:nTo], y0=pnorm(df$x[1:nTo]))
  segments(x0=-4, x1=df$x[nTo], y0=pnorm(df$x[nTo]), col="red", lwd=2)
  segments(x0=df$x[nTo], y0=0, y1=pnorm(df$x[nTo]), col="red", lwd=1)
  #hisgorgram of values
  h <- hist(df$x[1:nTo], breaks=seq(-4,4,.5), plot=F)
  h2 <- hist(df$x[nTo], breaks=seq(-4,4,.5), plot=F)
  hist(df$x[1:nTo], breaks=seq(-4,4,.5), main="x", xaxt="n",yaxt="n")
  ri <- which(h2$counts==1)
  rect(h2$breaks[ri], (h$counts[ri]-1),h2$breaks[ri+1], h$counts[ri], col="red")
  #plot histogram of F(X)
  #hist(df$px[1:nTo], breaks=seq(0,1,.1), main="F(x)", xaxt="n", yaxt="n")
  h <- hist(df$px[1:nTo], breaks=seq(0,1,.1),  plot=F)
  h2 <- hist(df$px[nTo], breaks=seq(0,1,.1), plot=F)
  hist(df$px[1:nTo], breaks=seq(0,1,.1), main="F(x)", xaxt="n",yaxt="n")
  ri <- which(h2$counts==1)
  rect(h2$breaks[ri], (h$counts[ri]-1),h2$breaks[ri+1], h$counts[ri], col="red")
  dev.off()
  #clear()
}


library(gifski)
png_files <- list.files(pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "animation.gif", delay = .1)

#off()
#detach(device)

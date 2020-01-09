library(Hmisc)

source('metagol_learning_rate_data.r', local=T)
source('louise_learning_rate_data.r', local=T)

# Line types and colours
plot.type <- 'b'
lin.typs <- 1:2
pnt.typs <- 1:2
systems.cols <- c('red','blue')

# Plot title and labels - mostly unused to make space for more plot
title <- 'Predictive accuracy comparison'
title.metagol <- 'Metagol accuracy'
title.louise <- 'Louise accuracy'
title.plain <- 'Predictive accuracy and sampling rate'
subtitle <- ''
x.lab <- 'Sampling rate' 
y.lab <- 'Accuracy'

# Legend
leg.text <- c('Metagol', 'Louise')
leg.lin.cols <- systems.cols
leg.lin.typs <- lin.typs
leg.pnt.typs <- pnt.typs
leg.lwd <- 4.0

# Error bar line and point types
bar.type = 'o' # try 'b'
rand.bar.col = 'gray48'
red.bar.col = 'magenta'
bas.bar.col = 'darkgreen'

# Increased axis, ticks, label, line and point sizes
# Better for papers.
cex.axis <- 2.70
cex.lab <- 2.8
cex <- 2.5
lwd.ticks=3.0
lwd <- 3.0
# Increased legend text size
leg.cex <- 3
# Increased errorbar sizes.
cap <- 0.025

results.length <- length(metagol.eval.mean)
x.axis <- metagol.sampling.rates

# Calculate standard errors.
metagol.eval.se <- metagol.eval.sd / sqrt(results.length)
louise.eval.se <- louise.eval.sd / sqrt(results.length)

# Get size of legend to add to y-axis limit
# Taken from:
# https://stackoverflow.com/questions/8929663/r-legend-placement-in-a-plot
plot.new()
leg.size <- legend('topleft', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, cex=leg.cex, lwd=leg.lwd, plot=F)

y.lim.max <- max(metagol.eval.mean+metagol.eval.se, louise.eval.mean+louise.eval.se) + 0.1 # Space for legend
y.lim.min <- min(metagol.eval.mean-metagol.eval.se, louise.eval.mean-louise.eval.se)
# Note legend size added to y max limit multiplied by a small factor to move it a little above the limit of the plot
y.lim <- c(y.lim.min, 1.05 * y.lim.max + leg.size$rect$h)
x.lim <- c(1, results.length + 0.5)

p <- par()
par(mar=c(5.3,6.1,1.0,0.8), mgp=c(4,1,0) )

plot(x.axis, metagol.eval.mean, ylim=y.lim, type=plot.type, lty=lin.typs[1], pch=pnt.typs[1], col=systems.cols[1], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, louise.eval.mean, ylim=y.lim, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

errbar(x.axis, metagol.eval.mean, yplus=metagol.eval.mean+metagol.eval.se, yminus=metagol.eval.mean-metagol.eval.se, col=0, pch=1, type=bar.type, errbar.col=red.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, louise.eval.mean, yplus=louise.eval.mean+louise.eval.se, yminus=louise.eval.mean-louise.eval.se, col=0, pch=1, type=bar.type, errbar.col=bas.bar.col, add=T, cap=cap, lwd=lwd)

axis(1, at=x.axis, labels=x.axis, cex.axis=cex.axis, cex.lab=cex.lab, padj=0.5, lwd.ticks=lwd.ticks)

legend('topleft', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)

par(p)

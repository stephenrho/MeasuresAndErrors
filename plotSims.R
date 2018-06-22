# plotSims.R
# Author: Stephen Rhodes (rhodessp at missouri.edu)
# License: GNU GPL v3.0

# Function for plotting the results of simulations. Example given at the end.
# Rhodes, Cowan, Parra, Logie "Interaction Effects on Common Measures of Sensitivity: Choice of Measure, Type I Error and Power"

plotSims <- function(data, xaxis, xgrid, ygrid, xaxisLab, xgridLab, ygridLab, yaxislab = 'Proportion Sig. Interactions', colors = NULL, shapes = c(15, 17, 16), linePos=0.05, xaxisLabLine = 4, xgridLabLine = 2.3, ygridLabLine = 2.7, xyaxisValsCex = .7, xgridValsLine = .7, ygridValsLine = .7, gridValsCex = .7, gridLabsCex = 1, leg = T, legPanel = 1, legy = .9){
  
  if (is.null(colors)){
    if(!require(viridisLite, quietly = T)){
      install.packages("viridisLite", quiet = T)
    }
    library(viridisLite, quietly = T)
    colors = rev(viridis(3, begin = .2, end=.8))
  }
  
  # extract levels for the grid
  xlevels = levels(as.factor(data[, xgrid]))
  ylevels = levels(as.factor(data[, ygrid]))
  # set graphical pars
  par(mar = c(0,0,.3,.3), oma = c(4, 3, 4, 4)+1, mfrow = c(length(ylevels), length(xlevels)))
  # range for x-axis (little wider than usual)
  xrange = range(data[,xaxis])
  xlim = xrange + c(-1,1)*(xrange[2] - xrange[1])*.1
  
  for (i in 1:length(ylevels)){
    for (j in 1:length(xlevels)){
      ### create empty plot
      plot(100, ylim = c(0,1), xlim=xlim, axes = F)
      box()
      # error line
      segments(x0 = xlim[1], x1 = xlim[2], y0 = linePos, y1 = linePos, col = 'grey')
      ### add points
      plotData = data[data[,xgrid] == xlevels[j] & data[,ygrid] == ylevels[i],]
      # d
      points(x = plotData[,xaxis], y = plotData[,'BxW.dp'], type='b', col = colors[1], pch= shapes[1], lty=2)
      # A'
      points(x = plotData[,xaxis], y = plotData[,'BxW.Ap'], type='b', col = colors[2], pch= shapes[2])
      # Pr
      points(x = plotData[,xaxis], y = plotData[,'BxW.Pr'], type='b', col = colors[3], pch= shapes[3], lty=3)
      ### add values/ axes
      if (ylevels[i] == ylevels[length(ylevels)]){
        axis(side = 1, at = unique(data[,xaxis]), cex.axis=xyaxisValsCex)
      }
      if (xlevels[j] == xlevels[1]){
        axis(side = 2, at = seq(0,1,.2), las=1, cex.axis=xyaxisValsCex)
      }
      # top label
      if (ylevels[i] == ylevels[1]){
        mtext(text = xlevels[j], side = 3, line = ygridValsLine, cex=gridValsCex)
      }
      # right side label
      if (xlevels[j] == xlevels[length(xlevels)]){
        mtext(text = ylevels[i], side = 4, line = xgridValsLine, cex=gridValsCex)
      }
      # add a legend to top left plot if requested
      if (xlevels[j] == xlevels[1] & ylevels[i] == ylevels[legPanel] & leg){          
        # determine how spaced out the legend should be
        legSpacex = (xrange[2] - xrange[1])*.2
        legSpacey = .2
        # d'
        points(x = xrange[1], y = legy, pch = shapes[1], col = colors[1], cex=1.5)
        text(x = xrange[1] + legSpacex, y = legy, labels = bquote(italic("d'")))
        # A'
        points(x = xrange[1], y = legy - legSpacey, pch = shapes[2], col = colors[2], cex=1.5)
        text(x = xrange[1] + legSpacex, y = legy - legSpacey, labels = bquote(italic("A'")))
        # Pr
        points(x = xrange[1], y = legy - 2*legSpacey, pch = shapes[3], col = colors[3], cex=1.5)
        text(x = xrange[1] + legSpacex, y = legy - 2*legSpacey, labels = bquote(italic(P[r])))
      }
    }
  }
  # Add axis/ grid labels
  mtext(text = xaxisLab, side = 1, line = xaxisLabLine, outer = T, cex=gridLabsCex)
  mtext(text = xgridLab, side = 3, line = xgridLabLine, outer = T, cex=gridLabsCex)
  mtext(text = ygridLab, side = 4, line = ygridLabLine, outer = T, cex=gridLabsCex)
  mtext(text = yaxislab, side = 2, line = 2.5, outer = T, cex=gridLabsCex)
}

# sim1_EVSDT <- read.table(file = 'results/tables/sim1_EVSDT')
# plotSims(data = sim1_EVSDT, xaxis = 'd_0', xgrid = 'N_s', ygrid = 'd_1', xaxisLab = 'd_0', xgridLab = 'N', ygridLab = 'd_1')

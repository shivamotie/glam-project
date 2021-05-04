#######################################################################
# 
# Fonction R permettant d'afficher la modalite de la variable
# categorielle a laquelle appartient chaque observation sur le
# diagramme de dispersion multivarie.
#
#######################################################################
# Last modified: 20.08.2012.
#######################################################################
# Warranty: none.
#######################################################################

PairsLab = function(data, labels, col="blue", ...){
	pairs(data,
        panel=function(x, y){
          points(x, y, type="n", ...)
          text(x, y, labels, col=col, ...)
        })
}

#######################################################################
# Example
#######################################################################
# 
# Retax = read.csv("../Data/Retax.csv")
# 
# par(mfcol=c(1,1), pty="s")
# 
# PairsLab(Retax[,c(1:4,7)],
#          labels=c(rep("R",41),rep("A",25)))
# 
# PairsLab(Retax[,c(1:4,7)],
#          labels=c(rep("R",41),rep("A",25)),
#          col=c(rep("blue",41),rep("darkorange3",25)),
#          cex.axis=1.3,cex.lab=1.3, cex=1.3, cex.main=1.3)
# 
#######################################################################



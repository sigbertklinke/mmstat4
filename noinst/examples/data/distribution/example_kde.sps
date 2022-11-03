DATASET ACTIVATE DatenSet1.
* Diagrammerstellung.
GGRAPH
/GRAPHDATASET NAME="DatenSet1" VARIABLES=MEDV
/GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
SOURCE:  s    = userSource(id("DatenSet1"))
DATA:    diag = col(source(s), name("MEDV"))
GUIDE:   axis(dim(2), label("Density"))
GUIDE:   axis(dim(1), label("Median houseprice"))
ELEMENT: line(position(density.kernel.epanechnikov(
diag,fixedWindow(0.1))))
END GPL.

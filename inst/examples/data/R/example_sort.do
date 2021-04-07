clear all
use bostonh.dta
// sort data by MEDV
list MEDV
sort MEDV
browse MEDV
list MEDV
//sort by MEDV und CRIM (no unique sorting order)
sort MEDV CRIM
list MEDV CRIM

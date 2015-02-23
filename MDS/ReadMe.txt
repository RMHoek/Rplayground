The function 'MultiDimensionalScaling.R uses two other functions:
-indexInVector.R
-moveIndexToFront.R

The function takes an nxn matrix representing a distance table and returns the coordinates of all points.

The function takes a few params:
-originAtIndex/originAtName: this is either the index number or name of the column representing the point at the origin. If not supplied column 1 is at the origin. The 'Name' param must be in the column names of the matrix.

-nextIndexWest/westByName: this is the index number or name of the column representing the point due west (with coord (x,0) with x>0). If not supplied column 2 is taken as the point due west. The 'Name' param must be in the column names of the matrix.

-nextIndexNorth/northByName: this is the index number or name of the column representing the point north of the origin (with coord (x,y) with y>0). If not supplied column 3 is taken as the point north. The 'Name' param must be in the column names of the matrix.

-withplot: either TRUE or FALSE; determins whether the function also plots the points as output

The function coordsToDistTab.R calculates the distance table from a set of coordinates.

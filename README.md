# plateLayout

Plate layout defintions are used to identify well types and
    locations on 384 or 96 well plates. This package provides convenience
    functionality for reading writing and querying layout definitions.

### Plate_Layout Reference CLass

Easy handling of layout read/writes from text or file.  

```R
PL <- Plate_Layout("384_Layout_1_Hcontrols_positives10cells_noGDNA.txt"
PL
# Reference object of class "Plate_LayoutRefClass" 
#    nwell: 384 
#    dim: [ 16, 24 ] 
#       1 cell: 315 
#       10 cells: 3 
#       No drop: 66 

typ <- PL$get_types()
head(typ)
#       A01       A02       A03       A04       A05       A06 
# "No drop"  "1 cell"  "1 cell"  "1 cell"  "1 cell"  "1 cell" 
```


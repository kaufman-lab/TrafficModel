# TrafficModel

## BRANCH:  `wip-docs`
_Add documentation to project and clean up for production._

# Notes

1. load roads from shapefiles in WGS84 proj (where are these from?) 
2. filter to area of interest plus 50km buffer
3. break up by A1/2/3
4. break into line segments 
5. load appropriate nocov_log model (do we need the others?)
6. calc midpoints of each seg (still in WGS84 space)
7. predict on those midpoints, then exponentiate 
8. get UTM coordinates for end points of segments (keep as X_b/X_e/Y_b/Y_e)
9. Save off a file like : 

```
       X_b     Y_b      X_e     Y_e     X_mid    Y_mid         X        Y   fitted dCL sigmaz0
1 438046.7 4599982 438047.2 4599881 -87.74284 41.54865 -87.74284 41.54865 94153.65   0       2
2 438047.2 4599881 438051.4 4599416 -87.74278 41.54610 -87.74278 41.54610 93423.70   0       2
3 438051.4 4599416 438051.3 4599254 -87.74272 41.54328 -87.74272 41.54328 92973.03   0       2
4 438016.3 4599983 438017.0 4599870 -87.74320 41.54860 -87.74320 41.54860 94201.31   0       2
  #lanes Hw1 dw1 Hw2 dw2 Depth Wtop Wbottom Group Z_b Z_e
1      4   0   0   0   0     0    0       0    G1   1   1
2      4   0   0   0   0     0    0       0    G1   1   1
3      4   0   0   0   0     0    0       0    G1   1   1
4      4   0   0   0   0     0    0       0    G1   1   1
```

every column after 'fitted' is constant.

10. Run RLINE!
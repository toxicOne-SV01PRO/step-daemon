G92 E0

G28 ; home all
G21 ; set units to millimeters
G90 ; use absolute coordinates
M82 ; use absolute distances for extrusion

M114

G1 F300
G1 X80 Y80
G1 X120 Y120
G1 X80 Y120
G1 X80 Y60
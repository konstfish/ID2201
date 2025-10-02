# DSM Ring Performance Heatmaps - Grid Style
# Save this as heatmaps.gnuplot and run with: gnuplot heatmaps.gnuplot

# ==============================================================================
# ADD LATENCY HEATMAP
# ==============================================================================

set terminal pngcairo size 900,700 enhanced font 'Arial,11'
set output 'add_latency_heatmap.png'

set title "Add Operation Latency (ms)" font "Arial,16" offset 0,-0.5

# Set up grid layout
set xrange [0.5:6.5]
set yrange [0.5:5.5]
set xtics ("1" 1, "2" 2, "4" 3, "8" 4, "16" 5, "32" 6) scale 0
set ytics ("1" 1, "2" 2, "4" 3, "8" 4, "16" 5) scale 0
set xlabel "Number of Machines" offset 0,0.5
set ylabel "Number of Nodes" offset 1.5,0

unset key
set grid front
set size square

# Color palette matching web version
set palette defined (0 '#10b981', 15 '#84cc16', 30 '#eab308', 45 '#f97316', 60 '#ef4444', 75 '#dc2626')
set cbrange [0:75]
set cblabel "Latency (ms)" offset -1,0

set view map
unset dgrid3d
set pm3d map interpolate 0,0
set pm3d corners2color c1

# Data for rectangles: xcenter ycenter latency
$data_add << EOD
1 1 2.0
2 1 5.0
3 1 15.25
4 1 25.5
5 1 25.75
1 2 6.0
2 2 13.5
3 2 17.5
4 2 25.0
5 2 29.69
2 3 7.5
3 3 10.0
4 3 25.25
5 3 28.5
3 4 24.25
4 4 40.5
5 4 30.56
6 4 49.88
4 5 42.63
5 5 66.56
6 5 71.63
EOD

# Plot rectangles as boxes
set style fill solid 1.0 border rgb "white"
splot $data_add using 1:2:3:3 with pm3d

# Add text labels centered in each box
set label 1 "2.0" at 1,1 center front tc rgb "black" font "Arial,10"
set label 2 "5.0" at 2,1 center front tc rgb "black" font "Arial,10"
set label 3 "15.3" at 3,1 center front tc rgb "black" font "Arial,10"
set label 4 "25.5" at 4,1 center front tc rgb "black" font "Arial,10"
set label 5 "25.8" at 5,1 center front tc rgb "black" font "Arial,10"
set label 6 "6.0" at 1,2 center front tc rgb "black" font "Arial,10"
set label 7 "13.5" at 2,2 center front tc rgb "black" font "Arial,10"
set label 8 "17.5" at 3,2 center front tc rgb "black" font "Arial,10"
set label 9 "25.0" at 4,2 center front tc rgb "black" font "Arial,10"
set label 10 "29.7" at 5,2 center front tc rgb "black" font "Arial,10"
set label 11 "7.5" at 2,3 center front tc rgb "black" font "Arial,10"
set label 12 "10.0" at 3,3 center front tc rgb "black" font "Arial,10"
set label 13 "25.3" at 4,3 center front tc rgb "black" font "Arial,10"
set label 14 "28.5" at 5,3 center front tc rgb "black" font "Arial,10"
set label 15 "24.3" at 3,4 center front tc rgb "black" font "Arial,10"
set label 16 "40.5" at 4,4 center front tc rgb "white" font "Arial,10"
set label 17 "30.6" at 5,4 center front tc rgb "black" font "Arial,10"
set label 18 "49.9" at 6,4 center front tc rgb "white" font "Arial,10"
set label 19 "42.6" at 4,5 center front tc rgb "white" font "Arial,10"
set label 20 "66.6" at 5,5 center front tc rgb "white" font "Arial,10"
set label 21 "71.6" at 6,5 center front tc rgb "white" font "Arial,10"

replot

# ==============================================================================
# LOOKUP LATENCY HEATMAP
# ==============================================================================

unset label
set output 'lookup_latency_heatmap.png'
set title "Lookup Operation Latency (ms)" font "Arial,16" offset 0,-0.5
set cbrange [0:60]

$data_look << EOD
1 1 3.0
2 1 4.0
3 1 5.75
4 1 3.875
5 1 17.56
1 2 1.0
2 2 4.0
3 2 6.0
4 2 8.5
5 2 18.31
2 3 6.5
3 3 19.5
4 3 12.63
5 3 22.0
3 4 10.75
4 4 26.38
5 4 26.19
6 4 43.41
4 5 26.13
5 5 33.56
6 5 59.09
EOD

splot $data_look using 1:2:3:3 with pm3d

set label 1 "3.0" at 1,1 center front tc rgb "black" font "Arial,10"
set label 2 "4.0" at 2,1 center front tc rgb "black" font "Arial,10"
set label 3 "5.8" at 3,1 center front tc rgb "black" font "Arial,10"
set label 4 "3.9" at 4,1 center front tc rgb "black" font "Arial,10"
set label 5 "17.6" at 5,1 center front tc rgb "black" font "Arial,10"
set label 6 "1.0" at 1,2 center front tc rgb "black" font "Arial,10"
set label 7 "4.0" at 2,2 center front tc rgb "black" font "Arial,10"
set label 8 "6.0" at 3,2 center front tc rgb "black" font "Arial,10"
set label 9 "8.5" at 4,2 center front tc rgb "black" font "Arial,10"
set label 10 "18.3" at 5,2 center front tc rgb "black" font "Arial,10"
set label 11 "6.5" at 2,3 center front tc rgb "black" font "Arial,10"
set label 12 "19.5" at 3,3 center front tc rgb "black" font "Arial,10"
set label 13 "12.6" at 4,3 center front tc rgb "black" font "Arial,10"
set label 14 "22.0" at 5,3 center front tc rgb "black" font "Arial,10"
set label 15 "10.8" at 3,4 center front tc rgb "black" font "Arial,10"
set label 16 "26.4" at 4,4 center front tc rgb "white" font "Arial,10"
set label 17 "26.2" at 5,4 center front tc rgb "white" font "Arial,10"
set label 18 "43.4" at 6,4 center front tc rgb "white" font "Arial,10"
set label 19 "26.1" at 4,5 center front tc rgb "white" font "Arial,10"
set label 20 "33.6" at 5,5 center front tc rgb "white" font "Arial,10"
set label 21 "59.1" at 6,5 center front tc rgb "white" font "Arial,10"

replot

# ==============================================================================
# STORAGE BALANCE HEATMAP
# ==============================================================================

unset label
set output 'storage_balance_heatmap.png'
set title "Storage Balance Ratio (Max/Min)" font "Arial,16" offset 0,-0.5

# Use linear scale for better color discrimination, but with special handling for 482
set palette defined (0 '#10b981', 5 '#84cc16', 15 '#eab308', 30 '#f97316', 60 '#ef4444', 100 '#dc2626')
set cbrange [0:100]
set cblabel "Balance Ratio" offset -1,0

$data_balance << EOD
1 1 1.0
2 1 1.0
3 1 1.0
4 1 1.0
5 1 1.0
1 2 3.67
2 2 1.03
3 2 1.12
4 2 1.71
5 2 5.93
2 3 8.17
3 3 2.42
4 3 4.81
5 3 13.14
3 4 20.88
4 4 11.92
5 4 13.83
6 4 100
4 5 66.67
5 5 19.57
6 5 31.66
EOD

splot $data_balance using 1:2:3:3 with pm3d

set label 1 "1.0" at 1,1 center front tc rgb "black" font "Arial,10"
set label 2 "1.0" at 2,1 center front tc rgb "black" font "Arial,10"
set label 3 "1.0" at 3,1 center front tc rgb "black" font "Arial,10"
set label 4 "1.0" at 4,1 center front tc rgb "black" font "Arial,10"
set label 5 "1.0" at 5,1 center front tc rgb "black" font "Arial,10"
set label 6 "3.7" at 1,2 center front tc rgb "black" font "Arial,10"
set label 7 "1.0" at 2,2 center front tc rgb "black" font "Arial,10"
set label 8 "1.1" at 3,2 center front tc rgb "black" font "Arial,10"
set label 9 "1.7" at 4,2 center front tc rgb "black" font "Arial,10"
set label 10 "5.9" at 5,2 center front tc rgb "black" font "Arial,10"
set label 11 "8.2" at 2,3 center front tc rgb "black" font "Arial,10"
set label 12 "2.4" at 3,3 center front tc rgb "black" font "Arial,10"
set label 13 "4.8" at 4,3 center front tc rgb "black" font "Arial,10"
set label 14 "13.1" at 5,3 center front tc rgb "black" font "Arial,10"
set label 15 "20.9" at 3,4 center front tc rgb "black" font "Arial,10"
set label 16 "11.9" at 4,4 center front tc rgb "black" font "Arial,10"
set label 17 "13.8" at 5,4 center front tc rgb "black" font "Arial,10"
set label 18 "482!" at 6,4 center front tc rgb "white" font "Arial Bold,11"
set label 19 "66.7" at 4,5 center front tc rgb "white" font "Arial,10"
set label 20 "19.6" at 5,5 center front tc rgb "black" font "Arial,10"
set label 21 "31.7" at 6,5 center front tc rgb "black" font "Arial,10"

replot

print ""
print "Generated three heatmap images:"
print "  - add_latency_heatmap.png"
print "  - lookup_latency_heatmap.png"
print "  - storage_balance_heatmap.png"
print ""

# DSM Ring Performance Heatmaps
# Save this as heatmaps.gnuplot and run with: gnuplot heatmaps.gnuplot

# ==============================================================================
# ADD LATENCY HEATMAP
# ==============================================================================

set terminal pngcairo size 800,600 enhanced font 'Arial,12'
set output 'add_latency_heatmap.png'

set title "Add Operation Latency (ms)" font "Arial,16"
set xlabel "Number of Machines"
set ylabel "Number of Nodes"

set xtics ("1" 1, "2" 2, "4" 4, "8" 8, "16" 16, "32" 32)
set ytics ("1" 1, "2" 2, "4" 4, "8" 8, "16" 16)

set palette defined (0 '#10b981', 0.2 '#84cc16', 0.4 '#eab308', 0.6 '#f97316', 0.8 '#ef4444', 1.0 '#dc2626')
set cbrange [0:75]
set cblabel "Latency (ms)"

set view map
set dgrid3d 5,6 splines
set pm3d interpolate 0,0

# Data: machines nodes latency
$data_add << EOD
1 1 2.0
2 1 5.0
4 1 15.25
8 1 25.5
16 1 25.75
1 2 6.0
2 2 13.5
4 2 17.5
8 2 25.0
16 2 29.69
2 4 7.5
4 4 10.0
8 4 25.25
16 4 28.5
4 8 24.25
8 8 40.5
16 8 30.56
32 8 49.88
8 16 42.63
16 16 66.56
32 16 71.63
EOD

splot $data_add using 1:2:3 with pm3d notitle

# Add text labels
set label 1 "2.0" at 1,1,2.0 center front
set label 2 "5.0" at 2,1,5.0 center front
set label 3 "15.3" at 4,1,15.25 center front
set label 4 "25.5" at 8,1,25.5 center front
set label 5 "25.8" at 16,1,25.75 center front
set label 6 "6.0" at 1,2,6.0 center front
set label 7 "13.5" at 2,2,13.5 center front
set label 8 "17.5" at 4,2,17.5 center front
set label 9 "25.0" at 8,2,25.0 center front
set label 10 "29.7" at 16,2,29.69 center front
set label 11 "7.5" at 2,4,7.5 center front
set label 12 "10.0" at 4,4,10.0 center front
set label 13 "25.3" at 8,4,25.25 center front
set label 14 "28.5" at 16,4,28.5 center front
set label 15 "24.3" at 4,8,24.25 center front
set label 16 "40.5" at 8,8,40.5 center front
set label 17 "30.6" at 16,8,30.56 center front
set label 18 "49.9" at 32,8,49.88 center front
set label 19 "42.6" at 8,16,42.63 center front
set label 20 "66.6" at 16,16,66.56 center front
set label 21 "71.6" at 32,16,71.63 center front

replot

# ==============================================================================
# LOOKUP LATENCY HEATMAP
# ==============================================================================

unset label
set output 'lookup_latency_heatmap.png'
set title "Lookup Operation Latency (ms)" font "Arial,16"
set cbrange [0:60]

$data_look << EOD
1 1 3.0
2 1 4.0
4 1 5.75
8 1 3.875
16 1 17.56
1 2 1.0
2 2 4.0
4 2 6.0
8 2 8.5
16 2 18.31
2 4 6.5
4 4 19.5
8 4 12.63
16 4 22.0
4 8 10.75
8 8 26.38
16 8 26.19
32 8 43.41
8 16 26.13
16 16 33.56
32 16 59.09
EOD

splot $data_look using 1:2:3 with pm3d notitle

set label 1 "3.0" at 1,1,3.0 center front
set label 2 "4.0" at 2,1,4.0 center front
set label 3 "5.8" at 4,1,5.75 center front
set label 4 "3.9" at 8,1,3.875 center front
set label 5 "17.6" at 16,1,17.56 center front
set label 6 "1.0" at 1,2,1.0 center front
set label 7 "4.0" at 2,2,4.0 center front
set label 8 "6.0" at 4,2,6.0 center front
set label 9 "8.5" at 8,2,8.5 center front
set label 10 "18.3" at 16,2,18.31 center front
set label 11 "6.5" at 2,4,6.5 center front
set label 12 "19.5" at 4,4,19.5 center front
set label 13 "12.6" at 8,4,12.63 center front
set label 14 "22.0" at 16,4,22.0 center front
set label 15 "10.8" at 4,8,10.75 center front
set label 16 "26.4" at 8,8,26.38 center front
set label 17 "26.2" at 16,8,26.19 center front
set label 18 "43.4" at 32,8,43.41 center front
set label 19 "26.1" at 8,16,26.13 center front
set label 20 "33.6" at 16,16,33.56 center front
set label 21 "59.1" at 32,16,59.09 center front

replot

# ==============================================================================
# STORAGE BALANCE HEATMAP
# ==============================================================================

unset label
set output 'storage_balance_heatmap.png'
set title "Storage Balance Ratio (Max/Min)" font "Arial,16"
set cbrange [1:100]
set logscale cb
set cblabel "Balance Ratio (log scale)"

$data_balance << EOD
1 1 1.0
2 1 1.0
4 1 1.0
8 1 1.0
16 1 1.0
1 2 3.67
2 2 1.03
4 2 1.12
8 2 1.71
16 2 5.93
2 4 8.17
4 4 2.42
8 4 4.81
16 4 13.14
4 8 20.88
8 8 11.92
16 8 13.83
32 8 482.05
8 16 66.67
16 16 19.57
32 16 31.66
EOD

splot $data_balance using 1:2:3 with pm3d notitle

set label 1 "1.0" at 1,1,1.0 center front
set label 2 "1.0" at 2,1,1.0 center front
set label 3 "1.0" at 4,1,1.0 center front
set label 4 "1.0" at 8,1,1.0 center front
set label 5 "1.0" at 16,1,1.0 center front
set label 6 "3.7" at 1,2,3.67 center front
set label 7 "1.0" at 2,2,1.03 center front
set label 8 "1.1" at 4,2,1.12 center front
set label 9 "1.7" at 8,2,1.71 center front
set label 10 "5.9" at 16,2,5.93 center front
set label 11 "8.2" at 2,4,8.17 center front
set label 12 "2.4" at 4,4,2.42 center front
set label 13 "4.8" at 8,4,4.81 center front
set label 14 "13.1" at 16,4,13.14 center front
set label 15 "20.9" at 4,8,20.88 center front
set label 16 "11.9" at 8,8,11.92 center front
set label 17 "13.8" at 16,8,13.83 center front
set label 18 "482!" at 32,8,482.05 center front tc rgb "red"
set label 19 "66.7" at 8,16,66.67 center front
set label 20 "19.6" at 16,16,19.57 center front
set label 21 "31.7" at 32,16,31.66 center front

replot

print "Generated three heatmap images:"
print "  - add_latency_heatmap.png"
print "  - lookup_latency_heatmap.png"
print "  - storage_balance_heatmap.png"

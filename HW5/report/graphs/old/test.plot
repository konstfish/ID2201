set term postscript eps enhanced color
set output "requests.eps"
set title "Request Delay Over Time"
set xlabel "Request Number"
set ylabel "Delay (milliseconds)"
set datafile separator ","

set yrange [0:60]

# Mark where 2-node configuration starts
dataset2_start = system("wc -l < requests-1node.csv") + 0 - 1
set arrow from dataset2_start, graph 0 to dataset2_start, graph 1 nohead linewidth 1 dashtype 2
set label "2 Nodes Start" at dataset2_start+2, graph 0.965 center

# Mark where 8-node configuration starts
dataset3_start = dataset2_start + (system("wc -l < requests-2node.csv") + 0 - 1)
set arrow from dataset3_start, graph 0 to dataset3_start, graph 1 nohead linewidth 1 dashtype 2
set label "8 Nodes Start" at dataset3_start+5, graph 0.965 center

plot "requests-1node.csv" using 1:2 skip 1 with linespoints title "1 Node - Add", \
     "requests-2node.csv" using 1:2 skip 1 with linespoints title "2 Nodes - Add", \
     "requests-8node.csv" using 1:2 skip 1 with linespoints title "8 Nodes - Add", \
     "requests-lookup.csv" using 1:2 skip 1 with linespoints title "Lookup"
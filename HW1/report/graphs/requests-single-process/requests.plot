set term postscript eps enhanced color
set output "requests.eps"
set title "Request Delay Over Time"
set xlabel "Time (seconds)"
set ylabel "Delay (microseconds)"
set datafile separator ","

set yrange [38000:100000]

dataset2_start = (system("head -2 requests-2.csv | tail -1 | cut -d',' -f1") + 0 - 1756986764125506) / 1000000.0
set arrow from dataset2_start, graph 0 to dataset2_start, graph 1 nohead linewidth 1 dashtype 2
set label "Bench 2 Start" at dataset2_start+0.675, graph 0.965 center


plot "requests-1.csv" using (($1-1756986764125506)/1000000):3 skip 1 with linespoints title "Bench 1", \
     "requests-2.csv" using (($1-1756986764125506)/1000000):3 skip 1 with linespoints title "Bench 2", \

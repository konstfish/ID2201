set term postscript eps enhanced color
set output "jitter.eps"
#set title "Request Delay Over Time"
set xlabel "Jitter (milliseconds)"
set ylabel "Maximum Queue Size"
set datafile separator ","

set yrange [0:20]

plot "jitter_lamp.csv" using 1:2 with linespoints title "Lamport", \
     "jitter_vect.csv" using 1:2 with linespoints title "Vector",


set term postscript eps enhanced color
set output "sleep.eps"
#set title "Request Delay Over Time"
set xlabel "Sleep (milliseconds)"
set ylabel "Maximum Queue Size"
set datafile separator ","

set yrange [0:26]

plot "sleep_lamp.csv" using 1:2 with linespoints title "Lamport", \
     "sleep_vect.csv" using 1:2 with linespoints title "Vector"

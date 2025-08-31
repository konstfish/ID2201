set term postscript eps enhanced color
set output "requests.eps"
set title "Request Delay Over Time"
set xlabel "Time (seconds)"
set ylabel "Delay (microseconds)"
set datafile separator ","

plot "requests-1.csv" using (($1-1756665779039086)/1000000):3 skip 1 with linespoints title "Dataset 1", \
     "requests-2.csv" using (($1-1756665779039086)/1000000):3 skip 1 with linespoints title "Dataset 2"
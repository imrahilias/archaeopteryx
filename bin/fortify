#!/bin/zsh
## fortify @ moritz siegel 210222

now = $(date "+%y%m%d%H%M")


## initialise fortran source file
echo "
! moritz siegel @ $now

program $1

  implicit none
  integer, parameter :: p=selected_real_kind( 16 )
  integer ::
  real( kind=p ) ::
  real( kind=p ), dimension( , ) ::



end program $1
" > $1.f90


## initialise gnuplot file
echo "
reset
set key
set size square
set term png size 1920,1080
set out $1.png
set title $1
#set cbr [1e-5:]
#set xrange [-10:10]
#set xrange [-10:10]
set xlabel 'x'
set ylabel 'y'
p $1.dat u 1:2
" > $1.gp


## compile & execute & plot 
echo "time gfortran -o $1 $1.f95 && time ./$1 && time gnuplot $1.gp" > $1.sh

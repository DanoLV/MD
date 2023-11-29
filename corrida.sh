#!/bin/bash

# Complilo el codigo fortran
make
rm datos*.dat
rm positions*.xyz
rm datosEstadisticaT.dat
clear

# Temperaturas
T=1.1

# Densidad
densidad=(0.001 0.01 0.1 0.2 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.8 0.9 1.0)

# Pasos de montecarlo por temperatura
pasomd=1000

# Pasos para termalizar en la primera corrida
pasosterm=1000

# Pasos cada cuanto se guardan datos en archivos de salida
pasossave=100

# Ejecusion en funsion de cambio de densidad a T cte
for i in ${!densidad[@]};
do
./md_g3b -nmd $pasomd -nmdt $pasosterm -nsave $pasossave -T $T -d ${densidad[$i]} -punto $i
done

# Proceso los datos
python3 estadistica.py -o datosEstadisticaT.dat -cantarch ${#densidad[@]}

# # muestro los graficos
# python3 plotDatosT.py -i datosEstadisticaT.dat 
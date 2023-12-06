#!/bin/bash

# Complilo el codigo fortran
make
rm *.dat
rm *.xyz
clear

#-------------------------------------------------------------------------------------------------------------
# Parametros generales 
#-------------------------------------------------------------------------------------------------------------
# Pasos de montecarlo 
pasomd=500000
# Pasos para termalizar
pasosterm=1000000
# Pasos minimizar potencial
pasosminu=50000
# Pasos de RDF
pasordf=0
# Pasos cada cuanto se guardan datos en archivos de salida
pasossave=500

#-------------------------------------------------------------------------------------------------------------
# Presión en función de la densidad  
#-------------------------------------------------------------------------------------------------------------
#Temperatura
T=1.1
# Densidades
densidad=(0.001 0.01 0.1 0.2 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.8 0.9 1.0)

# Ejecución en función de cambio de densidad a T cte
for i in ${!densidad[@]};
do
./md_g3b -nmd $pasomd -nrdf $pasordf -nmdt $pasosterm -nmu $pasosminu -nsave $pasossave -T $T -d ${densidad[$i]} -punto $i -od 'datosDensidad' -op 'posicionesDensidad'
done

# Proceso los datos
python3 estadisticaDensidad.py -o datosEstadisticaDensidad.dat -cantarch ${#densidad[@]} -i 'datosDensidad'
#-------------------------------------------------------------------------------------------------------------
# # Para ver el grafico
# python3 plotDatosD.py -i datosEstadisticaT.dat 
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
# Presión en función de la temperatura
#-------------------------------------------------------------------------------------------------------------
# Densidad
densidad=0.3
# Temperaturas
T=()
for ((i=0;i<=10;i++))
do
    Tn=$(echo "0.7+0.07*$i"|bc -l) 
    T+=($Tn)
done

# Ejecución en función de cambio de T a densidad cte  
for i in ${!T[@]};
do
./md_g3b -nmd $pasomd -nrdf $pasordf -nmdt $pasosterm -nmu $pasosminu -nsave $pasossave -T ${T[$i]} -d $densidad -punto $i -od 'datosT' -op 'posicionesT'
done

# Proceso los datos
python3 estadisticaT.py -o datosEstadisticaT.dat -cantarch ${#T[@]} -i 'datosT'

#-------------------------------------------------------------------------------------------------------------
# # muestro los graficos
# python3 plotDatosT.py -i datosEstadisticaT.dat 
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
# Funcion de distribucion radial
#-------------------------------------------------------------------------------------------------------------
# # Pasos de montecarlo 
# pasomd=0

# # Pasos para termalizar
# pasosterm=1000000

# # Pasos minimizar potencial
# pasosminu=20000

# # Pasos de RDF
# pasordf=200000

# # Pasos cada cuanto se guardan datos en archivos de salida
# pasossave=500
# #--------------------------------------------
# # Densidad
# densidad=(0.001 0.3 0.3 0.8)

# # Temperatura
# T=(1.1 1.1 1.4 1.1)
# #--------------------------------------------
# for i in ${!T[@]};
# do
# ./md_g3b -nmd $pasomd -nrdf $pasordf -nmdt $pasosterm -nmu $pasosminu -nsave $pasossave -T ${T[$i]} -d ${densidad[$i]} -punto 0 -od "datosT(${T[$i]})D(${densidad[$i]})" -op 'posicionesT'
# done
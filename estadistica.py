import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

# Establecer parametros de linea de comando
argParser = argparse.ArgumentParser()
argParser.add_argument("-cantarch", help="cantidad de archivos de datos")
argParser.add_argument("-o", help="Archivo de salida")

# Leer parametros de linea de comando
try: 
    args = argParser.parse_args()
except:
    sys.exit("No se pudo leer parametros de linea de comandos")

cant = int(args.cantarch)
if(cant <=0 ):
    sys.exit("La cantidad de archivos no es valida")

fout = args.o
if( fout is None):
    sys.exit('Especifique nombre de archivo de salida')

# Abrir archivo de salida
fo = open(fout, "w")
separador = ' '

for i in range(0,cant,1):
    # Leer archivo de datos
    archivo = "datos"+ str(i) + ".dat"
    y = np.genfromtxt(archivo)

    # Procesar datos
    T = np.mean(y[:,4])
    densidad = np.mean(y[:,5])
    presion = np.mean(y[:,6])
    varpresion = np.var(y[:,6])

    T = str(T)
    densidad = str(densidad)
    presion = str(presion)
    varpresion = str(varpresion)

    # Escribir datos en archivo de salida
    fo.write(T+separador+densidad+separador+presion+separador+varpresion+"\n")
    
# Cerrar archivo de salida
fo.close()
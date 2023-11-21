import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

# # Establecer parametros de linea de comando
# argParser = argparse.ArgumentParser()
# argParser.add_argument("-i", help="archivo de datos")

# # Leer parametros de linea de comando
# try: 
#     args = argParser.parse_args()
# except: 
#     sys.exit("No se pudo leer parametros de linea de comandos")

# # Verificar que se paso un nombre de archivo y salir si no fue asi
# archivo = args.i
# if( archivo is None):
#     sys.exit('Especifique nombre de archivo')

# Establecer subplots
figure, axis = plt.subplots(1,1) 

# axis[a,b].set_ylabel('$<E>_{sitio}$')
# axis[a,b].set_xlabel('T')
# axis[a,b].grid(True, which='both')
# Leer datos del archivo
y = np.genfromtxt('energia.dat')

# Construir los subplot de los datos
a=0
b=0
axis.plot(y[:,0],y[:,1])
axis.set_ylabel('$<E>_{sitio}$')
axis.set_xlabel('T')
axis.grid(True, which='both')

# axis[a,b].plot(y[:,0],y[:,1])
# axis[a,b].set_ylabel('$<E>_{sitio}$')
# axis[a,b].set_xlabel('T')
# axis[a,b].grid(True, which='both')
# axis[a,b].set_xlim([xmin,5])

# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)
mng = plt.get_current_fig_manager()
mng.resize(*mng.window.maxsize())
plt.show()
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
figure, axis = plt.subplots(1) 

# axis[a,b].set_ylabel('$<E>_{sitio}$')
# axis[a,b].set_xlabel('T')
# axis[a,b].grid(True, which='both')
# Leer datos del archivo
y = np.genfromtxt('energia.dat')

# Construir los subplot de los datos
a=0
b=0
axis.plot(y[:,0],y[:,1],'b')
axis.plot(y[:,0],y[:,2],'r')
axis.plot(y[:,0],y[:,3],'g')
axis.plot(y[:,0],y[:,4],'black')
# axis.set_xlim([9900000000,10000000000])
# axis.set_ylim([-0.5,3])
# axis.set_ylabel('$<Epotencial>_{sitio}$')
# axis.set_xlabel('Nmd')
axis.grid(True, which='both')
# axis[a].plot(y[:,0],y[:,1])
# axis[a].set_ylabel('$<Epotencial>_{sitio}$')
# axis[a].set_xlabel('Nmd')
# axis[a].grid(True, which='both')
# axis.set_ylim([0,15])

# a=1
# b=0
# axis[a].plot(y[:,0],y[:,2])
# axis[a].set_ylabel('$<Ecinetica>_{sitio}$')
# axis[a].set_xlabel('Nmd')
# axis[a].grid(True, which='both')

# a=2
# b=0
# axis[a].plot(y[:,0],(y[:,1]+y[:,2]))
# axis[a].set_ylabel('$<Et>_{sitio}$')
# axis[a].set_xlabel('Nmd')
# axis[a].grid(True, which='both')

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
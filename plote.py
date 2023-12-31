import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

# Establecer subplots
figure, axis = plt.subplots(1) 

# axis[a,b].set_ylabel('$<E>_{sitio}$')
# axis[a,b].set_xlabel('T')
# axis[a,b].grid(True, which='both')
# Leer datos del archivo
y = np.genfromtxt('datosT0.dat')

# Construir los subplot de los datos
a=0
b=0
axis.plot(y[:,0],y[:,1],'r')
# axis.plot(y[:,0],y[:,2],'b')
# axis.plot(y[:,0],y[:,3],'g')
axis.plot(y[:,0],y[:,4],'black')
axis.plot(y[:,0],y[:,5],'g')
axis.plot(y[:,0],y[:,6])
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
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
y = np.genfromtxt('datosEstadisticaT.dat')

a=0
b=0
# axis.plot(y[:,0],y[:,0],'r')
axis.plot(y[:,0],y[:,2],'b')

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



# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)# a=1
# b=0
# axis[a].plot(y[:,0],y[:,2])
# axis[a].set_ylabel('$<Ecinetica>_{sitio}$')
# axis[a].set_xlabel('Nmd')
# axis[a].grid(True, which='both')


mng = plt.get_current_fig_manager()
# mng.resize(*mng.window.maxsize())
plt.show()
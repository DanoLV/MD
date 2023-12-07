import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

# Establecer subplots
figure, axis = plt.subplots(1,2) 

y = np.genfromtxt('datosEstadisticaT.dat')

a=0
axis[a].plot(y[:,0],y[:,2],'b')
axis[a].set_ylabel('P')
axis[a].set_xlabel('T')
axis[a].grid(True, which='both')

a=1
axis[a].plot(y[:,0],y[:,3],'b')
axis[a].set_ylabel('$\mathrm{\sigma}_P$')
axis[a].set_xlabel('T')
axis[a].grid(True, which='both')

# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)
mng = plt.get_current_fig_manager()
mng.resize(*mng.window.maxsize())
plt.show()
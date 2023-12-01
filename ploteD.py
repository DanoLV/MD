import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

# Establecer subplots
figure, axis = plt.subplots(1) 

y = np.genfromtxt('datosEstadisticaDensidad.dat')

# Construir los subplot de los datos
a=0
b=0
axis.plot(y[:,1],y[:,1],'r')
axis.plot(y[:,1],y[:,2],'b')

# axis.set_xlim([9900000000,10000000000])
# axis.set_ylim([-0.5,3])
# axis.set_ylabel('$<Epotencial>_{sitio}$')
# axis.set_xlabel('Nmd')
axis.grid(True, which='both')

# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)
mng = plt.get_current_fig_manager()
# mng.resize(*mng.window.maxsize())
plt.show()
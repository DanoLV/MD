import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

plt.rc('axes', labelsize=30) 
plt.rc('ytick', labelsize=24)
plt.rc('xtick', labelsize=24)

# Establecer subplots
figure, axis = plt.subplots(1,2) 

y = np.genfromtxt('datosEstadisticaT.dat')

a=0
axis[a].plot(y[:,0],y[:,2],'b')
axis[a].set_ylabel('P')
axis[a].set_xlabel('T')
axis[a].grid(True, which='both',linewidth=3,linestyle='--')
axis[a].tick_params(width=3)
for spine in ['top','bottom','left','right']:
    axis[a].spines[spine].set_linewidth(3)

a=1
axis[a].plot(y[:,0],y[:,3],'b')
axis[a].set_ylabel('$\mathrm{\sigma}_P$')
axis[a].set_xlabel('T')
axis[a].grid(True, which='both',linewidth=3,linestyle='--')
axis[a].tick_params(width=3)
for spine in ['top','bottom','left','right']:
    axis[a].spines[spine].set_linewidth(3)

# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)
mng = plt.get_current_fig_manager()
mng.resize(*mng.window.maxsize())
plt.show()
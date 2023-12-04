import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys
from scipy.interpolate import Rbf

# Establecer subplots
figure, axis = plt.subplots(1) 

# axis[a,b].set_ylabel('$<E>_{sitio}$')
# axis[a,b].set_xlabel('T')
# axis[a,b].grid(True, which='both')
# Leer datos del archivo
yd = np.genfromtxt('rdfdatosT(1.1)D(0.8)0.dat')

# Construir los subplot de los datos
x = yd[:,0]
y = yd[:,1]
cubic_interpolation_model = Rbf(x, y,function='inverse')
 
# Plotting the Graph
X_=np.linspace(x.min(), x.max(), 1000)
Y_=cubic_interpolation_model(X_)

axis.plot(X_,Y_,'r')
# axis.set_xlim([0,x[20]])
# axis.set_ylim([-0.5,3])
axis.set_ylabel('g(r)')
axis.set_xlabel('r')
axis.grid(True, which='both')

# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)
mng = plt.get_current_fig_manager()
mng.resize(*mng.window.maxsize())
plt.show()
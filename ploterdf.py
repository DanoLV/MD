import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys
from scipy.interpolate import Rbf

# Establecer subplots
figure, axis = plt.subplots(1) 

#------------------------------------------
# Leer datos del archivo
y1 = np.genfromtxt('rdfdatosT(1.1)D(0.001)0.dat')
y2 = np.genfromtxt('rdfdatosT(1.1)D(0.3)0.dat')
y3 = np.genfromtxt('rdfdatosT(1.4)D(0.3)0.dat')
y4 = np.genfromtxt('rdfdatosT(1.1)D(0.8)0.dat')
#------------------------------------------
# Construir los subplot de los datos
x1 = y1[:,0]
interpolation1 = Rbf(x1, y1[:,1],function='inverse')#,function='inverse') linear multiquadric

x2 = y2[:,0]
interpolation2 = Rbf(x2, y2[:,1],function='inverse')#,function='inverse') linear

x3 = y3[:,0]
interpolation3 = Rbf(x3, y3[:,1],function='inverse')#,function='inverse') linear

x4 = y4[:,0]
interpolation4 = Rbf(x4, y4[:,1],function='inverse')#,function='inverse') linear
#------------------------------------------
# Plotting the Graph
X1_=np.linspace(x1.min(), x1.max(), 2500)
Y1_=interpolation1(X1_)

X2_=np.linspace(x2.min(), x2.max(), 1000)
Y2_=interpolation2(X2_)

X3_=np.linspace(x3.min(), x3.max(), 1000)
Y3_=interpolation3(X3_)

X4_=np.linspace(x4.min(), x4.max(), 1000)
Y4_=interpolation4(X4_)
#------------------------------------------

axis.plot(X1_,Y1_,'r')
axis.plot(X2_,Y2_,'g')
axis.plot(X3_,Y3_,'b')
axis.plot(X4_,Y4_,'k')
axis.set_ylabel('g(r)')
axis.set_xlabel('r')
axis.grid(True, which='both')
axis.set_xlim(0,4)

# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)
mng = plt.get_current_fig_manager()
mng.resize(*mng.window.maxsize())
plt.show()
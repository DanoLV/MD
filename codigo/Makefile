MAKEFILE = Makefile
exe = md_g3b
fcomp = gfortran #ifort # /opt/intel/compiler70/ia32/bin/ifc  
# Warning: the debugger doesn't get along with the optimization options
# So: not use -O3 WITH -g option
flags =  -O3 -fopenmp
# Remote compilation -fopenmp
OBJS = ziggurat.o mdrutinas3b.o md_g3b.o

.SUFFIXES:            # this deletes the default suffixes 
.SUFFIXES: .f90 .o    # this defines the extensions I want 

.f90.o:  
	$(fcomp) -c -g $(flags) $< 
        

$(exe):  $(OBJS) Makefile 
	$(fcomp) $(flags) -o $(exe) $(OBJS) 


clean:
	rm ./*.o ./*.mod	


MD_G3b.o: md_g3b.f90 ziggurat.o mdrutinas3b.o

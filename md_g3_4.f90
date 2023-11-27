! Guia 3: Dinamica Molecular

program md_g3
   use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit, stderr=>error_unit
   use globals
   use ziggurat
   use mdrutinas
   use omp_lib

   implicit none
   logical :: es
   integer :: seed,i,j,N,nmd, oute, outp, nstepscalc, nprevio, nequilibracion, nproc
   real(kind=8):: L,sigma,epsilon,u,fvec(3),rc2,dt,m,kb,media, densidad,ec,dte,dtm
   real(kind=8), allocatable ::r(:,:),f(:,:),v(:,:),vaux(:,:)
   character(20) :: filee, filep

!************************************************
   real :: start, finish
   call cpu_time(start)
!************************************************
![NO TOCAR] Inicializa generador de número random

   inquire(file='seed.dat',exist=es)
   if(es) then
      open(unit=10,file='seed.dat',status='old')
      read(10,*) seed
      close(10)
      ! print *,"  * Leyendo semilla de archivo seed.dat"
   else
      seed = 24583490
   end if

   call zigset(seed)
![FIN NO TOCAR]
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   nproc = OMP_get_max_threads()
   write(stdout,*) 'procesadores: ',nproc
   call omp_set_dynamic(.true.)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   filee = "energia.dat"
   filep = "positions.xyz"
   sigma= 1
   epsilon = 1
   rc2 = (2.5*sigma)**2
   m = 1
   dte= 0.005
   dtm = 0.00005
   nmd=500000
   kb=1
   densidad = 0.4
   nstepscalc = 10000
   nprevio = 10000
   nequilibracion = 100000

   ! Recibir parametros N, L
   L = 6
   N = NINT(densidad * L**3)

   ! NINTInicializar variables
   allocate(r(N,3))
   r=0

   allocate(f(N,3))
   f=0

   allocate(v(N,3))
   v=0
   allocate(vaux(N,3))
   vaux=0

   ! Inicializar vectores
   !Posiciones
   call Init_pos(N,L,r)

   !Velocidades
   media=0!1.5*epsilon/kb
   call Init_rand(N,real(1.0,8),v,'nor',media)

   !Fuerzas
   call fuerzas(f, r, N, sigma, epsilon, L, rc2)

   dt = dte
   !Loop de estabilizacion
   do j = 1, nprevio

      !calculo posiciones nuevas
      call pos_1(f, r, N, m, dt)! r(t+dt)

      !calculo fuerza nueva
      call fuerzas(f, r, N, sigma, epsilon, L, rc2)

   end do

   ! dt = dtm

   !Loop de equilibracion
   do j = 1, nequilibracion


      !calculo posiciones nuevas
      call pos_verlet(f, v, r, N, m, dt, L) ! r(t+dt)

      !v(t+ 1/2 dt)
      ! vaux = velocidadintermedia(f,v,m,N,dt)
      v = velocidadintermedia(f,v,m,N,dt)

      !calculo fuerza nueva
      call fuerzas(f, r, N, sigma, epsilon, L, rc2)

      !v(t+dt)
      ! v = velocidadintermedia(f,vaux,m,N,dt)
      v = velocidadintermedia(f,v,m,N,dt)

   end do

   !abro archivo para la energia
   oute=15
   OPEN(unit=oute,file=filee, status='replace', position='append')

   !abro archivo para las posiciones
   outp=4
   OPEN(unit=outp,file=filep, status='replace', position='append')
   call savePosInFile (r, N, outp)

   dt = dtm
   !Loop de MD
   do i = 1, nmd


      !calculo posiciones nuevas
      call pos_verlet(f, v, r, N, m, dt, L) ! r(t+dt)

      !v(t+ 1/2 dt)
      ! vaux = velocidadintermedia(f,v,m,N,dt)
      v = velocidadintermedia(f,v,m,N,dt)

      !calculo fuerza y potencial nuevos
      call calculos(u, f, r, N, sigma,epsilon, L,rc2) !f(t+dt)

      !v(t+dt)
      ! v = velocidadintermedia(f,vaux,m,N,dt)
      v = velocidadintermedia(f,v,m,N,dt)

      ! Saco datos
      if ( MOD(i,nstepscalc)== 0 ) then

         !Energia cinetica
         ec = calc_ecinetica(v,N,m)

         write(oute,*) (i*dtm+nequilibracion*dte+nprevio*dtm), ' ',u/N, ' ',ec/N, ' ',(u+ec)/N
         call savePosInFile (r, N, outp)

         write(stdout,*) "paso", i
      end if

   end do

   !Guardo ultimo punto
   !Energia cinetica
   ec = calc_ecinetica(v,N,m)
   call savePosInFile (r, N, outp)
   write(oute,*) (nmd*dtm+nequilibracion*dte+nprevio*dtm), ' ',u/N, ' ',ec/N, ' ',(u+ec)/N

   close(outp)
   close(oute)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios

   open(unit=10,file='seed.dat',status='unknown')
   seed = shr3()
   write(10,*) seed
   close(10)
![FIN no Tocar]

!************************************************
   call cpu_time(finish)
   ! print '("Time = ",f6.3," seconds.")',finish-start

!************************************************
end program

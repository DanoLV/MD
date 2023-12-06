! Guia 3: Dinamica Molecular

program md_g3
   use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit, stderr=>error_unit
   use globals
   use ziggurat
   use mdrutinas3b
   use omp_lib

   implicit none
   logical :: es
   integer :: seed,i,j,N,nmd, oute, outp, outrdf, nstepscalc, nprevio, nmdt, nproc, punto,nrdf,nr
   real(kind=8):: L,sigma,epsilon,u,fvec(3),rc2,dt,m,kb,media, densidad,ec,dte,dtm,T,Temp
   real(kind=8):: gama,presion, pvirial, deltar,raux
   real(kind=8), allocatable ::r(:,:),f(:,:),v(:,:),vaux(:,:),hrdf(:,:),auxrdf(:,:)
   character(len=50) :: filee, filep,str, filerdf
   integer :: num_args, ix, stat
   character(len=50), dimension(:), allocatable :: args
!************************************************
   ! real :: start, finish
!************************************************
   nrdf = 0
   nmdt = 0
   nmd = 0
   nstepscalc = 500
   nprevio = 0
!************************************************
! Manejar argumentos de linea de comandos
   num_args = command_argument_count()
   if ( num_args > 0 ) then


      allocate(args(num_args))

      do ix = 1, num_args

         call get_command_argument(ix,args(ix))

         ! Parser de opciones
         select case(args(ix))

            ! Densidad
          case('-d')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  densidad

            ! Temperatura
          case('-T')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  T

            ! Pasos de MD a ejecutar
          case('-nmd')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  nmd

            ! Pasos de MD para termalizar
          case('-nmdt')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  nmdt

            ! Pasos de minimizacion U
          case('-nmu')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  nprevio
            ! Pasos de minimizacion RDF
          case('-nrdf')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  nrdf

            !punto para nombre archivo
          case('-punto')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  punto

            !Pasos cada cuanto se guarda en salida
          case('-nsave')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  nstepscalc

            !Archivo salida datos
          case('-od')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  filee

            !Archivo salida trayectorias
          case('-op')
            call get_command_argument(ix + 1,args(ix + 1))
            read(args(ix+1),*,iostat=stat)  filep


         end select

      end do
   end if

!************************************************
   ! call cpu_time(start)
!************************************************
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
   ! nproc = OMP_get_max_threads()
   ! write(stdout,*) 'procesadores: ',nproc
   call omp_set_dynamic(.true.)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   print *, punto
   write(str,'(I0)') punto
   filee = trim(filee)  // trim(str) // ".dat" !"datos" // trim(str) // ".dat"
   filep =  trim(filep) // trim(str) // ".xyz" !"positions" // trim(str) // ".xyz"
   sigma= 1
   epsilon = 1
   kb=1
   m = 1
   gama = 0.5
   rc2 = (2.5*sigma)**2
   dte= 0.002
   dtm = 0.0001

   ! Recibir parametros N, L
   N = 200
   L = (real(N/densidad,8))**(1.0/3.0)


   ! Inicializar variables
   allocate(r(N,3))
   r=0

   allocate(f(N,3))
   f=0

   allocate(v(N,3))
   v=0
   allocate(vaux(N,3))
   vaux=0

   !abro archivo para la energia
   oute=15
   OPEN(unit=oute,file=filee, status='replace', position='append')

   !abro archivo para las posiciones
   outp=4
   OPEN(unit=outp,file=filep, status='replace', position='append')

   ! Inicializar vectores
   !Posiciones
   call Init_pos(N,L,r)

   !Fuerzas
   call fuerzas(f, r, N, sigma, epsilon, L, rc2)

!************************************************
   dt = dte
   !Loop de estabilizacion
   do j = 1, nprevio

      !calculo posiciones nuevas
      call pos_1(f, r, N, m, dt, L)! r(t+dt)

      !calculo fuerza nueva
      call fuerzas(f, r, N, sigma, epsilon, L, rc2)

   end do

!************************************************
   !Loop de termalizacion
   dt = dtm
   do j = 1, nmdt

      !calculo posiciones nuevas
      call pos_verlet(f, v, r, N, m, dt, L) ! r(t+dt)

      !calculo fuerza y potencial nuevos
      call fuerzas(f, r, N, sigma, epsilon, L, rc2)

      !Langevine
      call force_verlet(f, v, N, m, dt, T, gama)

      !v(t+dt)
      call velocidadverlet(f,v,m,N,dt)

   end do

! ************************************************
   !Loop de MD
   dt = dtm
   do i = 1, nmd

      !calculo (t+dt) y v(t+ 1/2 dt)
      call pos_verlet(f, v, r, N, m, dt, L)

      !calculo fuerza y potencial nuevos
      call calculos(u, f, r, N, sigma, epsilon, L,rc2, pvirial) !f(t+dt)

      !Langevine
      call force_verlet(f, v, N, m, dt, T, gama)

      !v(t+dt)
      call velocidadverlet(f,v,m,N,dt)

      ! Saco datos
      if ( MOD(i,nstepscalc)== 0 ) then

         !Energia cinetica
         ec = calc_ecinetica(v,N,m)
         Temp = 2*ec/(3*N*kb)
         presion = densidad*kb*Temp + 1/(3*L**3)*pvirial
         write(oute,*) (i*dtm+nmdt*dte+nprevio*dtm), ' ',u/N, ' ',ec/N, ' ',(u+ec)/N,'', Temp, densidad, presion
         call savePosInFile (r, N, outp)

      end if

   end do
   close(outp)
   close(oute)

!************************************************
   !Loop de rdf
   if ( nrdf > 0 ) then

      dt = dtm

      nr=ceiling(L/(0.05)) !200
      deltar = L/nr

      allocate(hrdf(2,nr))
      allocate(auxrdf(2,nr))
      hrdf=0.0
      i=0
      print *, nr, 3**(1.0/3.0) * L, nr*deltar

      do j = 1, nrdf

         !calculo posiciones nuevas
         call pos_verlet(f, v, r, N, m, dt, L) ! r(t+dt)

         !calculo fuerza y potencial nuevos
         call fuerzas(f, r, N, sigma, epsilon, L, rc2)

         !Langevine
         call force_verlet(f, v, N, m, dt, T, gama)

         !v(t+dt)
         call velocidadverlet(f,v,m,N,dt)

         if ( MOD(j,nstepscalc)== 0 ) then

            call rdf(r, N, L, auxrdf, nr, deltar)
            hrdf(2,:) = auxrdf(2,:) + hrdf(2,:)
            i= i+1

         end if

      end do

      hrdf(1,:) = auxrdf(1,:)

      raux = 4.0/3.0*densidad*(4.D0*DATAN(1.D0)) !4/3*densidad*pi
      hrdf(2,1)= hrdf(2,1)/(i*raux*(hrdf(1,1))**3)

      do j = 2, nr
         hrdf(2,j)= hrdf(2,j)/(i*raux*((hrdf(1,j))**3-(hrdf(1,j-1))**3))
      end do

      ! Sacar datos de rdf----------------------------------------------------------------------
      filerdf = "rdf" // trim(filee)
      print *, filerdf
      outrdf = 15
      OPEN(unit=outrdf,file=filerdf, status='replace', position='append')
      do i = 1, nr/2
         write(outrdf,*) hrdf(1,i), hrdf(2,i)
      end do
      close(outrdf)
      !----------------------------------------------------------------------------------------
   end if
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios

   open(unit=10,file='seed.dat',status='unknown')
   seed = shr3()
   write(10,*) seed
   close(10)
![FIN no Tocar]

!************************************************
   ! call cpu_time(finish)
   ! print '("Time = ",f6.3," seconds.")',finish-start

!************************************************
end program

! Guia 3: Dinamica Molecular

program md_g3
   use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit, stderr=>error_unit
   use globals
   use ziggurat
   use mdrutinas

   implicit none
   logical :: es
   integer :: seed,i,j,N,nmd, out, nstepscalc, nprevio, nequilibracion
   real(kind=8):: L,sigma,epsilon,u,fvec(3),rc2,dt,m,kb,media, densidad,ec,dte,dtm
   real(kind=8), allocatable ::r(:,:),f(:,:),v(:,:),vaux(:,:)

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

   sigma= 1
   epsilon = 1
   rc2 = (2.5*sigma)**2
   m = 1
   dte= 0.1
   dtm = 0.001
   nmd=100000
   kb=1
   densidad = 0.4
   nstepscalc = 100
   nprevio = 300000
   nequilibracion = 300000

   ! Recibir parametros N, L
   L = 10
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
      call pos_verlet(f, v, r, N, m, dt, L) ! r(t+dt)

      !calculo fuerza nueva
      call fuerzas(f, r, N, sigma, epsilon, L, rc2)

   end do

   out=4
   OPEN(unit=out,file='positions.xyz', status='replace', position='append')
   call savePosInFile (r, N, out)
   ! write(stdout,*) j*dt, ' ',u, ' ',ec

   dt = dtm

   !Loop de equilibracion
   do j = 1, nequilibracion


      !calculo posiciones nuevas
      call pos_verlet(f, v, r, N, m, dt, L) ! r(t+dt)

      !v(t+ 1/2 dt)
      vaux = velocidadintermedia(f,v,m,N,dt)

      !calculo fuerza nueva
      call fuerzas(f, r, N, sigma, epsilon, L, rc2)

      !v(t+dt)
      v = velocidadintermedia(f,vaux,m,N,dt)

   end do

   !Loop de MD
   do j = 1, nmd


      !calculo posiciones nuevas
      call pos_verlet(f, v, r, N, m, dt, L) ! r(t+dt)

      !v(t+ 1/2 dt)
      vaux = velocidadintermedia(f,v,m,N,dt)

      !calculo fuerza y potencial nuevos
      call calculos(u, f, r, N, sigma,epsilon, L,rc2) !f(t+dt)

      !v(t+dt)
      v = velocidadintermedia(f,vaux,m,N,dt)

      !Energia cinetica
      ec = calc_ecinetica(v,N,m)

      ! Saco datos
      if ( MOD(i,nstepscalc)== 0 ) then
         write(stdout,*) (j*dtm+nequilibracion*dte+nprevio*dtm), ' ',u, ' ',ec, ' ',(u+ec)
         call savePosInFile (r, N, out)
      end if

   end do
   ! write(stdout,*) (nequilibracion+nmd+nprevio+1)*dt, ' ',u, ' ',ec, ' ',u+ec
   close(out)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios

   open(unit=10,file='seed.dat',status='unknown')
   seed = shr3()
   write(10,*) seed
   close(10)
![FIN no Tocar]

end program

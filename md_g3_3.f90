! Guia 3: Dinamica Molecular

program md_g3

   use globals
   use ziggurat
   use mdrutinas

   implicit none
   logical :: es
   integer :: seed,i,j,N,nmd,  oute, outp, nstepscalc
   real(kind=8):: L,sigma,epsilon,u,rc2,dt,m
   real(kind=8), allocatable :: r(:,:), f(:,:)
   character(20) :: filee, filep

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
   filee = "energia.dat"
   filep = "positions.xyz"
   sigma= 1
   epsilon = 1
   rc2 = (2.5*sigma)**2
   m = 1

   dt = 0.05
   nmd=500000
   nstepscalc= 500

   ! Recibir parametros N, L
   N = 100
   L = 10

   ! Inicializar variables
   allocate(r(N,3))
   r=0.0

   allocate(f(N,3))
   f=0.0

   ! Inicializar vectores
   call Init_pos(N,L,r)
   ! call Init_rand(N,L,r)

   call calculos(u,f, r, N, sigma,epsilon, L,rc2)

   !abro archivo para la energia
   oute=15
   OPEN(unit=oute,file=filee, status='replace', position='append')

   !abro archivo para las posiciones
   outp=4
   OPEN(unit=outp,file=filep, status='replace', position='append')
   call savePosInFile (r, N, outp)

   !Loop de MD
   do j = 1, nmd

      !calculo posiciones nuevas
      ! do i = 1, N
      !    r(i,:) = r(i,:) + 0.5* f(i,:)/m*dt**2
      ! end do
      call pos_1(f, r, N, m, dt, L)

      !calculo fuerza y potencial nuevos
      call calculos(u,f, r, N, sigma,epsilon, L,rc2)
      ! call fuerzas(f, r, N, sigma, epsilon, L, rc2)
      ! Saco datos
      if ( MOD(j,nstepscalc)== 0 ) then

         write(oute,*) j, ' ',u/N
         call savePosInFile (r, N, outp)

      end if

   end do

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

end program

! Guia 3: Dinamica Molecular

program md_g3

   use globals
   use ziggurat
   use mdrutinas

   implicit none
   logical :: es
   integer :: seed,i,j,N,nmd, out
   real(kind=8):: L,sigma,epsilon,u,rc2,dt,m
   real(kind=8), allocatable ::r(:,:),f(:,:)

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
   rc2 = (4*sigma)**2
   m = 1
   dt = 0.5
   nmd=100000

   ! Recibir parametros N, L
   N = 20
   L = 20

   ! Inicializar variables
   allocate(r(N,3))
   r=0

   allocate(f(N,3))
   f=0

   ! Inicializar vectores
   ! call Init_pos(N,L,r)
   call Init_rand(N,L,r)

   call calculos(u,f, N,r, sigma,epsilon, L,rc2)

   out=4
   OPEN(unit=out,file='positions.xyz', status='replace', position='append')
   call savePosInFile (r, N, out)

   !Loop de MD
   do j = 1, nmd

      !calculo posiciones nuevas
      do i = 1, N
         r(i,:) = r(i,:) + 0.5* f(i,:)/m*dt**2
      end do
      call savePosInFile (r, N, out)

      !calculo fuerza y potencial nuevos
      call calculos(u,f, N,r, sigma,epsilon, L,rc2)

   end do

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

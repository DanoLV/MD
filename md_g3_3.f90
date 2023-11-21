! Guia 3: Dinamica Molecular

program md_g3

   use globals
   use ziggurat
   use mdrutinas

   implicit none
   logical :: es
   integer :: seed,i,j,N,nmd
   real(kind=8):: L,sigma,epsilon,u,fvec(3),rc2,dt,m
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
   rc2 = (2.5*sigma)**2
   m = 1
   dt = 0.1
   nmd=100

   ! Recibir parametros N, L
   N = 2
   L = 9

   ! Inicializar variables
   allocate(r(N,3))
   r=0
   ! allocate(v(N,3))
   allocate(f(N,3))
   f=0

   ! Inicializar vectores
!    call Init_pos(N,L,r)
   ! call Init_pos_rand(N,L,r)

   r(1,:)= [4,0,0]
   r(2,:)= [5.0,0.0,0.0]

   ! do i = 1, N
   !    print*, r(i,:)
   ! end do

   call calculos(u,f, N,r, sigma,epsilon, L,rc2)
   ! print *, u
   ! do i = 1, N
   !    print*, f(i,:)
   ! end do

   !Loop de MD
   do j = 1, nmd
      print *, 'paso:',j
      !calculo posiciones nuevas
      do i = 1, N
         r(i,:) = r(i,:) + 0.5* f(i,:)/m*dt**2
      end do
      ! do i = 1, N
      !    print*, r(i,:)
      ! end do

      !calculo fuerza y potencial nuevos
      call calculos(u,f, N,r, sigma,epsilon, L,rc2)
      print *, u
      ! do i = 1, N
      !    print*, f(i,:)
      ! end do
   end do

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios

   open(unit=10,file='seed.dat',status='unknown')
   seed = shr3()
   write(10,*) seed
   close(10)
![FIN no Tocar]

end program

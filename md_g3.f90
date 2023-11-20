! Guia 3: Dinamica Molecular

program md_g3 

    use globals
    use ziggurat
    use mdrutinas

    implicit none
    integer :: i,N
    real(kind=8):: L,sigma,epsilon,u,f(3),rc2
    real(kind=8), allocatable ::r(:,:)

    sigma= 1
    epsilon = 1
    rc2 = 100

    ! Recibir parametros N, L
    N = 9
    L = 9

    ! Inicializar variables
    allocate(r(N,3))
    ! allocate(v(N,3))
    ! allocate(f(N,3))

    ! Inicializar vectores
    call Init_pos(N,L,r)
    ! call Init_vel(N,L,v)
    ! call calc_force(N,L,r,v,f)

    ! do i = 1, N
    !     print*, r(i,:)
    ! end do

    ! call V_interaccion(u,N,r,sigma,epsilon)
    ! print *, u

    f = fuerza(r(1,:), r(2,:), sigma, epsilon, rc2)
    print *, f

    ! Ciclos de MD

end program
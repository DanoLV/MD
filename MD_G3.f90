! Guia 3: Dinamica Molecular

program MD_G3 

    use globals
    use ziggurat
    use MDRutinas

    implicit none
    integer :: i
    ! Recibir parametros N, L
    N = 9
    L = 9
    print*, N,L

    ! Inicializar variables
    allocate(r(N,3))
    ! allocate(v(N,3))
    ! allocate(f(N,3))

    ! Inicializar vectores
    call Init_pos_rand(N,L,r)
    ! call Init_vel(N,L,v)
    ! call calc_force(N,L,r,v,f)

    do i = 1, N
        print*, r(i,:)
    end do

    ! Ciclos de MD

end program
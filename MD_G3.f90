! Guia 3: Dinamica Molecular

program MD_G3 
    use Globals
    use ziggurat
    use MDRutinas

    implicit none

    ! Recibir paratros N, L

    allocate(r(3,N))
    allocate(v(3,N))
    allocate(f(3,N))

    ! Inicializar vectores
    call Init_pos(N,L,r)
    call Init_vel(N,L,v)
    call calc_force(N,L,r,v,f)

    ! Ciclos de MD

end program
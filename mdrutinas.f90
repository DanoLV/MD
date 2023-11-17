! Rutinas generales de MD
MODULE mdrutinas
    use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit, stderr=>error_unit
    use ziggurat
    use Globals
    IMPLICIT NONE

 CONTAINS

 SUBROUTINE Init_pos(N,L,r)

    integer, intent(in)  :: N
    real (kind=8), intent(in) :: L
    real (kind=8), allocatable :: r(:,:)
    real(kind=8):: dl, dN
    integer :: p, i, j, k, fin

    ! Particulas que entran en un lado L
    dN = CEILING(N**(Real(1.0/3.0))) !!!! PROBLEMA. Redondeo a int superior.

    ! Lado de un volumen de particula en la caja de lado L
    dl = L / dN
    fin = INT(dN )
    p = 1
    i=0
        do while (i .lt. fin .and. p .le. N) 
            j=0
            do while (j .lt. fin .and. p .le. N) 
                k = 0
                do while (k .lt. fin .and. p .le. N) 
                    r(p,:) = [REAL(i*dl)+dl/2, REAL(j*dL)+dL/2, REAL(k*dL)+dL/2]
                    p = p + 1
                    k = k + 1
                end do
                j = j + 1
            end do
            i = i + 1
        end do    

 END SUBROUTINE Init_pos

 SUBROUTINE Init_pos_rand(N,L,r)

    integer, intent(in)  :: N
    real (kind=8), intent(in) :: L
    real (kind=8), allocatable :: r(:,:)
    integer :: p
    
    do p = 1, N
        r(p,:) = L* [uni(), uni(), uni()]
    end do

 END SUBROUTINE Init_pos_rand

 SUBROUTINE V_interaccion(u,p1,p2,sigma,epsilon)
    REAL(kind=8), intent(in):: sigma,epsilon,p1(3),p2(3)
    REAL(kind=8):: u

    u = U_r(p1,p2,sigma,epsilon)

 END SUBROUTINE U_interaccion

 REAL(kind=8) FUNCTION U_r(p1,p2,sigma,epsilon)
    REAL(kind=8), intent(in):: p1(3),p2(3),sigma,epsilon
    REAL(kind=8) :: r 

    !Calculo distancia entre particulas
    r = sqrt((p1(1)-p2(1))**2+(p1(2)-p2(2))**2+(p1(3)-p2(3))**2)

    U_r = 4.0*epsilon*(-(sigma/r)**6.0+(sigma/r)**12.0)

 END FUNCTION U_r

END MODULE mdrutinas
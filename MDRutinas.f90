! Rutinas generales de MD
MODULE MDRutinas
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
    fin = INT(dN -1)
    do p = 1, N
        do i = 0, fin
            do j = 0, fin
                do k = 0, fin
                    r(p,:) = [REAL(i*dl)+dl/2, REAL(j*dL)+dL/2, REAL(k*dL)+dL/2]
                end do
            end do
        end do    
    end do

 END SUBROUTINE Init_pos

END MODULE MDRutinas
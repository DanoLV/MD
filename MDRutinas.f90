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
    integer :: p, i, j, k

    ! Particulas que entran en un lado L
    dN = INT(N**(Real(1.0/3.0))) !!!! PROBLEMA
    ! Lado de un volumen de particula en la caja de lado L
    dl = L / dN

    p=1
    do while(p <= N)
        do i = 1, dN
            r(i,p) = [dl/2, dL/2, dL/2]
        end do
    end do

 END SUBROUTINE Init_pos

END MODULE MDRutinas
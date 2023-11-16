! Variables globales
MODULE globals

    IMPLICIT NONE
    integer, public :: N
    real(kind=8), allocatable , public:: r(:,:), v(:,:), f(:,:)
    real(kind=8), public:: L

end MODULE globals
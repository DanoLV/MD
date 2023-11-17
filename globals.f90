! Variables globales
MODULE globals

    IMPLICIT NONE
    integer, public :: Nglo
    real(kind=8), allocatable , public:: rglo(:,:), vglo(:,:), fglo(:,:)
    real(kind=8), public:: Lglo, uglo, sigmaglo, epsilonglo

end MODULE globals
module glob

    use types
    
    integer (kind=ik), public :: a = 10
    
    
    real (kind=rk), public :: dt, end_t
    
    
    type :: geom_type
        integer (kind=ik) :: nx
        integer (kind=ik) :: ny
        real (kind=rk), dimension(:), allocatable :: dx
        real (kind=rk), dimension(:), allocatable :: dy
    end type geom_type
    
    
    type(geom_type), public :: geom

end module glob
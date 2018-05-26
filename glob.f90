module glob

    use types
    
    integer (kind=ik), public :: a = 10
    
    
    real (kind=rk), public :: dt, end_t
    
    
    type :: geom_type
        integer (kind=ik) :: ndx
        integer (kind=ik) :: ndy
        real (kind=rk), dimension(:), allocatable :: dx
        real (kind=rk), dimension(:), allocatable :: dy
    end type geom_type
    
    type(geom_type), public :: geom
    
    type :: lin_sys_type
        real (kind=rk), dimension(:,:), allocatable :: A
        real (kind=rk), dimension(:), allocatable :: b
        real (kind=rk), dimension(:), allocatable :: c
    end type lin_sys_type
    
    type(lin_sys_type), public :: lin_sys
    
    real(kind=rk), dimension(:,:), allocatable :: diff_coef
    real(kind=rk), dimension(:,:), allocatable :: bc_type
    
    real(kind=rk), dimension(1:2) :: adv
    

end module glob
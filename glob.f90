module glob

    use types
    
    integer (kind=ik), public :: a = 10
    
    
    real (kind=rk), public :: dt, end_t
    
   
    type(geom_type), public :: geom
   
    type(lin_sys_type), public :: lin_sys
    
    real(kind=rk), dimension(:,:), allocatable, public :: diff_coef
    real(kind=ik), dimension(:,:), allocatable, public :: bc_type
    
    real(kind=rk), dimension(1:2), public :: adv
    
    real(kind=rk), dimension(1:2), public :: diff_mult
    
    integer, public :: out_unit
!     
end module glob
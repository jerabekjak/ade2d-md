module glob

    use types
    
    integer (kind=ik), public :: a = 10
    
    
    real (kind=rk), public :: dt, end_t
    
   
    type(geom_type), public :: geom
   
    type(lin_sys_type), public :: lin_sys
    
    real(kind=rk), dimension(:,:), allocatable :: diff_coef
    real(kind=ik), dimension(:,:), allocatable :: bc_type
    
    real(kind=rk), dimension(1:2) :: adv
    
    real(kind=rk), dimension(1:2) :: diff_mult
    

end module glob
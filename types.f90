module types
 
    integer, parameter, public :: rk=selected_real_kind(15,99)
    integer, parameter, public :: ik=selected_int_kind(10)
    integer, parameter, public :: sik=selected_int_kind(1)
   
    type :: lin_sys_type
        integer(kind=ik) :: n
        integer(kind=ik) :: m
        integer(kind=ik) :: els
        real (kind=rk), dimension(:,:), allocatable :: A
        real (kind=rk), dimension(:), allocatable :: b
        real (kind=rk), dimension(:), allocatable :: c
    end type lin_sys_type
    
         
    type :: geom_type
        integer (kind=ik) :: ndx
        integer (kind=ik) :: ndy
        real (kind=rk), dimension(:), allocatable :: dx
        real (kind=rk), dimension(:), allocatable :: dy
    end type geom_type
   
   
   
end module types
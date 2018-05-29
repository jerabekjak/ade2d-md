program main
  use tools
  use solve
  
 
    call init_glob()

    call fill_lin_system()

  
!     test solver
!     call test_solver()
  

 contains
 
    subroutine test_solver()
        use types
        use solve
        integer(kind=ik) :: nel
        real(kind=rk), dimension(7,-2:2) :: A
        real(kind=rk), dimension(7   ) :: b
        real(kind=rk), dimension(7   ) :: x = 1._rk
        integer(kind=ik) :: n
        integer(kind=ik) :: m
        
        nel = 7
        n = 3
        m = 3
        
        A(1,:) = (/0.,0.,1.,0.,0./)
        A(2,:) = (/0.,0.,1.,0.,0./)
        A(3,:) = (/0.,0.,1.,0.,0./)
        A(4,:) = (/1.,1.,-2.,1.,1./)
        A(5,:) = (/0.,0.,1.,0.,0./)
        A(6,:) = (/0.,0.,1.,0.,0./)
        A(7,:) = (/0.,0.,1.,0.,0./)
        
        b = (/20.,1.,1.,1.,1.,1.,20./)
        
        call solver_gs(nel,A,b,x,n,m)
        
        print *, x
        print *, 'should be equal to'
        print *, '20.0  1.0  1.0 20.5  1.0  1.0 20.0'
    
    end subroutine 
 
end program main










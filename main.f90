program main
    use tools
    use solve
    use glob
    real(kind=rk) :: t=0._rk
    integer :: it = 0
    call init_glob()

    call fill_lin_system()
    
    
    do 
        if (t > end_t) exit
        call solver_gs(lin_sys%els,lin_sys%A,lin_sys%b,lin_sys%c,lin_sys%n,lin_sys%m)
        call fill_b()
        t = t + dt
        it=it + 1
        if (modulo(it,1000) == 0) then
            write(*,"(f10.5,a20)") t/end_t*100., ' % is calculated...'
        end if
    end do
    
    
    
    call prt_results(lin_sys,out_unit)
    
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










module solve

 contains
 
    subroutine solver_gs(nel,A,b,c,n,m)
        use types
        integer(kind=ik), intent(in) :: nel
        real(kind=rk), dimension(nel,-2:2), intent(in) :: A
        real(kind=rk), dimension(nel   ), intent(in) :: b
        real(kind=rk), dimension(nel   ), intent(inout) :: c
        integer(kind=ik), intent(in) :: n
        integer(kind=ik), intent(in) :: m
        
        integer(kind=ik) :: i, j, iter
        real(kind=rk)    :: sigma
        real(kind=rk)    :: err = 1.e-6
        real(kind=rk), dimension(size(c)) :: c_copy
        
        c_copy = c
        
        iter = 0
        
        do 
        
            do i = 1, nel
            
                sigma = 0._rk
                
                if (A(i,0) /= 1.) then ! if dirichlet bc
                  sigma = sigma + A(i,-2) * c(i-n)
                  sigma = sigma + A(i,-1) * c(i-1)
                  sigma = sigma + A(i, 1) * c(i+1)
                  sigma = sigma + A(i, 2) * c(i+n)
                end if 
                
                c(i) = 1./A(i,0)*(b(i) - sigma)
                
            end do
!             print *, sum((c-c_copy)**2.)
!             read(*,*)
            iter = iter + 1
            if (sum((c-c_copy)**2.)<err) then
!                 print *, iter
                exit
            end if
            
            c_copy = c
            
        end do
    
    end subroutine

end module solve
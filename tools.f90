module tools

 contains
 
    subroutine fill_lin_system()
        use types
        use glob
        
        real(kind=rk) :: b = 0.2
        integer :: i, n
        real(kind=rk), dimension(1:2) :: wrk_d
        
        
        
        do i = 1, lin_sys%els
        
            
            
          wrk_d  = det_diff(i)
!           print *, i, wrk_d
            
!             
!             vx = 
!             vy = 
            
            lin_sys%A(i,-2) = 1.
            lin_sys%A(i,-1) = 1.
            lin_sys%A(i, 0) = 1.
            lin_sys%A(i, 1) = 1.
            lin_sys%A(i, 2) = 1.
            
            
            lin_sys%b(i   ) = b/dt*lin_sys%c(i)
            
            
        end do
        
        
        
        
        
        
    end subroutine 
    
    function det_diff(iel)  result(d)
        use types
        use glob
        integer, intent(in)           :: iel
        real(kind=rk), dimension(1:2) :: d
        
        integer :: i, j
        real    :: x
        
        i = modulo(real(iel-1),real(geom%ndx+1))
        
        x = real(iel-1)/real(geom%ndx+1)
        j = x  ! this will cut the numbers after floating point to get the row index
        
        print *, iel, i, j
        
        
        d = diff_coef(i,j)
    
    
    end function 
    
    
    
    
 
 
 
 
   

    subroutine init_glob()
        use types
        use glob
        integer(kind=sik) :: ioerr
        character(len=64) :: config_file
        integer :: config_unit
        character(len=64) :: bc_file
        integer :: bc_unit
        character(len=64) :: diff_file
        integer :: diff_unit

        ! check got arguments
        if (iargc() == 0) then
            print *, 'no config file, eg.: ./ade2d conf.in'
            stop
        end if
        
        config_unit = 101 
        bc_unit = 102
        diff_unit = 103

        ! parsing + opening config file
        call getarg(1, config_file)
        open(config_unit, file=config_file, status='old', action='read', iostat=ioerr)
        print *, 'reading file ', trim(config_file), ' with iostat ', ioerr

        ! parsing parameters from config file
        call comment(config_unit)
        read(config_unit,*) geom%ndx
        call comment(config_unit)
        read(config_unit,*) geom%ndy
        
        ! allocating dx, dy arrays
        allocate(geom%dx(1:geom%ndx))
        allocate(geom%dy(1:geom%ndy))
        
        call comment(config_unit)
        read(config_unit,*) geom%dx
        
        call comment(config_unit)
        read(config_unit,*) geom%dy
        
        call comment(config_unit)
        read(config_unit,*) dt
        
        call comment(config_unit)
        read(config_unit,*) end_t
        
        call comment(config_unit)
        read(config_unit,*) diff_file
        open(diff_unit, file=diff_file, status='old', action='read', iostat=ioerr)
        call read_diff_coef(diff_unit)
        
        
        call comment(config_unit)
        read(config_unit,*) bc_file
        open(bc_unit, file=bc_file, status='old', action='read', iostat=ioerr)
        call read_bc_coef(bc_unit)
        
        call comment(config_unit)
        read(config_unit,*) adv
        
        call comment(config_unit)
        read(config_unit,*) diff_mult
        
        call lin_sys_alloc()
        
        


    end subroutine init_glob
   
   
   
    subroutine read_diff_coef(unit)
        use glob
        integer, intent(in) :: unit
        integer :: i
        
        allocate(diff_coef(0:geom%ndy,0:geom%ndx))
        
        do i = 0, geom%ndy
          read(unit,*) diff_coef(i,:)
        end do
        
    end subroutine read_diff_coef
    
    
    subroutine read_bc_coef(unit)
        use glob
        integer, intent(in) :: unit
        integer :: i
        
        allocate(bc_type(0:geom%ndy,0:geom%ndx))
        
        do i = 0, geom%ndy
          read(unit,*) bc_type(i,:)
        end do
        
    end subroutine read_bc_coef
    
    
    
    
    subroutine lin_sys_alloc()
        use glob
        
        allocate(lin_sys%A(1:((geom%ndy+1) * (geom%ndx+1)),-2:2))
        allocate(lin_sys%b(1:((geom%ndy+1) * (geom%ndx+1))))
        allocate(lin_sys%c(1:((geom%ndy+1) * (geom%ndx+1))))
        
        lin_sys%n = geom%ndy+1
        lin_sys%m = geom%ndx+1
        lin_sys%els = ((geom%ndy+1) * (geom%ndx+1))
    end subroutine lin_sys_alloc
   
    subroutine comment(unit)

        integer, intent(in) :: unit
        character(len=1) :: symbol
        character(len=1) :: string
        integer :: i_err

        symbol = "#"

        do
            read(unit=unit,fmt = *, iostat = i_err ) string
            if (i_err < 0) then
                return
            end if
            if (string == symbol) then
                continue
            else
                backspace unit
                return
            endif
        end do

    end subroutine comment


    end module tools
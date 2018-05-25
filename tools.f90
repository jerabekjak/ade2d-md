module tools

 contains 
   

    subroutine init_glob()
        use types
        use glob
        integer(kind=sik) :: ioerr
        character(len=64) :: config_file

        ! check got arguments
        if (iargc() == 0) then
            print *, 'no config file, eg.: ./ade2d conf.in'
            stop
        end if

        ! parsing + opening config file
        call getarg(1, config_file)
        open(101, file=config_file, status='old', action='read', iostat=ioerr)
        print *, 'reading file ', trim(config_file), ' with iostat ', ioerr

        ! parsing parameters from config file
        call comment(101)
        read(101,*) geom%nx
        call comment(101)
        read(101,*) geom%ny
        
        ! allocating dx, dy arrays
        allocate(geom%dx(1:geom%nx))
        allocate(geom%dy(1:geom%ny))
        
        call comment(101)
        read(101,*) geom%dx
        
        call comment(101)
        read(101,*) geom%dy
        
        call comment(101)
        read(101,*) dt
        
        call comment(101)
        read(101,*) end_t
        

        
        
        
        


    end subroutine init_glob
   
   
   
   
   
   
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
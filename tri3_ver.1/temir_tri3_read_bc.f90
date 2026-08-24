subroutine read_BC(datfile)
    use parameters
    implicit none

    !!!!! Read the same file as subroutine read_dat, and make vector of External Force and Given Displacement.

    ! Declear variable get from main

    ! Declear variable pssed by read_geometry
    integer, intent(in) :: datfile

    ! Declear common variable in read_bc
    character(len=256) :: line, field
    character(len=20)  :: bc_set_name(nbc_max)
    integer :: num_bc_set, bc_set_id, bc_node_set(nbc_max,1000)
    integer :: i, j, l

    ! Variables used in "define"
    character(len=256) :: bc_set_type
    character(len=20)  :: tokens(100)
    integer :: ini_node, fin_node, num_to_node, temp_node, token_count, set_node_count
    integer :: ios_temp_node
    logical :: find_to

    ! Variables used in fixed disp and point load
    integer :: fixed_disp_vector(nbc_max,3)
    integer :: num_stored_bc
    real(8) :: fixed_disp_magn(nbc_max,3), point_load_magn(nbc_max,2)
    real(8) :: magnitude

    !!! Go to the top of .dat input file
    rewind(datfile)
    bc_set_id = 1

    !!! Skip to first "define"
    do                                             
        read(datfile,"(A)") line
        if (line(1:12) == "define") then                            ! Find a block of "define"
            exit
        end if
    end do    

    !!! Get "set" info of boundary conditions
    do 
        read(line(21:40),*) bc_set_type                             ! Check the type of set of boundary condition
        
        if (bc_set_id > nbc_max) then
            print*, "!!!!! ERROR : Boundary conditions exist more than !!!!!", nbc_max
            error stop
        else
            ! No problem
        end if

        ! Confirm the type of set is "node". This program accept displacement or POINT LOAD as a boundary conditions.
        if (bc_set_type == "node") then
            ! No problem
        else
            print *, "!!!!! ERROR : THe type of boundary conditions are wrong !!!!! "
            error stop
        end if

        read(line(61:80),*) bc_set_name(bc_set_id)                  ! Store the name of set of BC 
        set_node_count = 1

        ! Loop A1 !
        do 
            read(datfile,"(A)") line
            tokens = "@"
            read(line,*) tokens                                     ! Sprit line to block and memorize as array of "tokens"
            token_count = count(tokens /= "@")                      ! Count th number of entry of "tokens"

            find_to = any(tokens(1:token_count) == "to")            ! Continuas nodes are expressed with "to".
            if (find_to) then
                read(tokens(1),*) ini_node                          ! Store initial node number of "to" list
                read(tokens(3),*) fin_node                          ! Store final node number of "to" list
                num_to_node = fin_node - ini_node + 1

                do l = 1, num_to_node                                           
                    bc_node_set(bc_set_id, l) = ini_node + l - 1    ! Store node number to apply BC
                end do

            else 
                ! Loop A2 !
                do i = 1, token_count
                    if (tokens(i) == "c") then                                  ! "c" means bc_node_set continue to the next line.
                        exit
                    else
                        read(tokens(i),*,iostat=ios_temp_node) temp_node        ! Try to store token as Integer
                        if (ios_temp_node == 0) then                            ! Integer means bc_node_set continue to next token. 
                            bc_node_set(bc_set_id, set_node_count) = temp_node  ! Store node number to apply BC
                            set_node_count = set_node_count + 1
                        else                                                    ! This line should contain nothing except integer or "c".
                            print *, "!!!!! ERROR : Failed to read bc_set !!!!!"    
                            error stop
                        end if
                    end if
                end do
            
                if(tokens(token_count) == "c") then                 ! If last token is "c", go to next line and repeat Loop A2.
                    ! Read next line
                else
                    exit                                            ! If last token is integer, this "define" block end.
                end if
            end if
        end do
        
        bc_set_id = bc_set_id + 1                                   
        read(datfile,"(A)") line
        if (line(1:12) == "define") then                            ! If next line start with "define", repeat Loop A1.
            ! Continuar do loop
        else                                                        ! All of "define" has finished.
            exit
        end if
    end do

    num_bc_set = bc_set_id - 1          ! This is used to specify times of loop to find bc_set_name
    bc_set_id = 1                       ! Already used in upper section. Need to be reset.
    num_stored_bc = 0                   ! This is used to judge whether all BC have been read.

    ! Loop B1 !
    Buscar_All_BC : do 
        read(datfile,"(A)") line
        
        !!! Read fixed disp
        if (line(1:12) == "fixed disp") then                        ! Find a block of "fixed disp"
            read(datfile,*)
            
            ! Loop B2(fixed disp) !
            do
                read(datfile,"(A)") line
                do l = 1, num_bc_set                                ! "l" is just a counter to search the same name.
                    if (line(61:80) == bc_set_name(l)) then         ! Find the set of "define" which has the same name 
                        bc_set_id = l
                        exit
                    end if
                end do

                read(datfile,"(A)") line
                tokens = "@"
                read(line,*) tokens                                 ! Sprit line to block and memorize as array of "tokens"
                token_count = count(tokens /= "@")                  ! Count th number of entry of "tokens"

                do i = 1, token_count
                    magnitude = expo2double(tokens(i))
                    fixed_disp_magn(bc_set_id, i) = magnitude       ! Store magnitude of displacment
                end do

                read(datfile,*)
                   
                read(datfile, "(A)") line
                tokens = "@"
                read(line,*) tokens

                do j = 1, token_count
                    read(tokens(j),*) fixed_disp_vector(bc_set_id, j)           ! Store vector of BC
                end do

                read(datfile,*)
                read(datfile,*)

                num_stored_bc = num_stored_bc + 1                   ! Count up the number of BC which are already read.
                if (num_stored_bc == num_bc_set) then               ! Judgement of end of BC to read
                    exit Buscar_All_BC
                else
                    ! Continuar reading BC
                end if                
            end do
        
        !!! Read point load
        else if (line(1:12) == "point load") then                   ! Find a block of "point load" 
            read(datfile,*)
            
            ! Loop B2(point load) !
            do
                read(datfile,"(A)") line
                do l = 1, num_bc_set                                ! "l" is just a counter to search the same name.
                    if (line(61:80) == bc_set_name(l)) then         ! Find the set of "define" which has the same name
                        bc_set_id = l
                        exit
                    end if
                end do

                read(datfile,"(A)") line
                tokens = "@"
                read(line,*) tokens                                 ! Sprit line to block and memorize as array of "tokens"
                token_count = count(tokens /= "@")                  ! Count th number of entry of "tokens"        

                do i = 1, token_count
                    magnitude = expo2double(tokens(i))
                    point_load_magn(bc_set_id, i) = magnitude       ! Store magnitude of point load 
                end do

                read(datfile,*)
                read(datfile,*)
                read(datfile,*)
                
                num_stored_bc = num_stored_bc + 1                   ! Count up the number of BC which are already read.
                if (num_stored_bc == num_bc_set) then               ! Judgement of end of BC to read
                    exit Buscar_All_BC
                else
                    ! Continuar reading BC
                end if               
            end do
        end if
    end do Buscar_All_BC                                            ! End of boundary conditions
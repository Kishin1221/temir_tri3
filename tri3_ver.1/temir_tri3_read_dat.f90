subroutine read_geometry(datfile, connect, coord, nnode, nelem, E, nu)
    use parameters
    implicit none

    !!!!! Get .dat file and extract model info and boundary conditions.

    ! Declear variable get from main
    integer, intent(out) :: datfile
    integer :: nnode, nelem
    integer :: connect(nelem_max, 3)
    real(8) :: coord(nnode_max, 2)
    real(8) :: E, nu

    ! Declear local variable in read_geometry
    character(len=256) :: datfilename
    character(len=256) :: line, field, bc_set_type 
    character(len=20)  :: bc_set_name(10)
    integer :: ios_nelem, ios_nnode, elem_type, ios_elem_type
    integer :: i, j, l, m, n, pos, expo, bc_set_id
    integer :: fixed_disp_vector(10,2), point_load_vector(10,2)
    real(8) :: fixed_disp_magn(10,2), point_load_magn(10,2)
    real(8) :: magnitude
    integer :: bc_node_set(10,3)

    

    !!! Specify .dat file to input
    call get_command_argument(1,datfilename)                        ! Get the name of input file
    print *, "Input file = ",trim(datfilename)
    open(newunit=datfile, file = trim(datfilename), status="old", action="read")

    !!! Get geometry info of model
    ! Get model size
    do 
        read(10,"(A)") line
        if (line(1:10) == "sizing") then                            ! Buscar "sizing"
            read(line(41:50),*,iostat=ios_nelem) nelem              ! Store number of elements
            read(line(51:60),*,iostat=ios_nnode) nnode              ! Store number of nodes
            
            ! Error Section 
            !! いずれ節点数や要素数の制限も設けたい．
            if(ios_nelem == 0 .and. ios_nnode == 0) then
                print *, "number of element = ", nelem
                print *, "number of node = ", nnode
            else
                print *, "!!!!! ERROR : Size data read !!!!!"
                print *, "line = ",line
            end if

            exit
        end if
    end do
  
    ! Store element connectivity
    do 
        read(10,"(A)") line
        if(line(1:12) == "connectivity") then                       ! Buscar "connectivity"
            read(10,*)

            do m = 1, nelem
                read(10,"(A)") line
                read(line(16:20),*,iostat=ios_elem_type) elem_type
                    if(elem_type == 201) then                       ! Confirm type of elemnt == 201

                        read(line(6:10),*) i                        ! Store element number
                                                                    ! The number of line "m" has possible to differ from element number "i".
                        read(line(26:30),*) connect(i, 1)           ! Store number of 1st node 
                        read(line(36:40),*) connect(i, 2)           ! Store number of 2nd node
                        read(line(46:50),*) connect(i, 3)           ! Store number of 3rd node

                    else
                        print *, "!!!!! Unexpected type of element : element number = ", "element type = ", i, elem_type
                    end if
            end do
            exit
        end if
    end do

    ! Store node coordinate
    do 
        read(10,"(A)") line
        if(line(1:11) == "coordinates") then                        ! Buscar "coordinates"
            read(10,*)
            do n = 1, nnode 
                read(10,"(A)") line
                read(line(6:10),*) j                                ! Store node number
                                                                    ! The number of line "n" has possible to differ from node number "j".
                do l = 1, 2
                    read(line(20*l-9:20*l+10),*) field              ! Read value of node in each axis
                    coord(j, l) = expo2double(field)                ! Calcular value of node in each axis
                end do
            end do
            exit
        end if
    end do

    !!! Get material properties
    !!暫定的なやつ．仕様書がよくわからない．
    do 
        read(10,"(A)") line
        if(line(1:9) == "isotropic") then
            read(10,*)
            read(10,*)
            read(10,*)
            
            read(10,"(A)") line
            read(line(1:20),*) field
            E = expo2double(field)                                  ! Convert notation by expo2double
            
            read(line(21:40),*) field
            nu = expo2double(field)                                 ! Convert notation by expo2double
            exit
        end if
    end do
             

    rewind(10)                                                      ! Start reading the file from the top
    bc_set_id = 1                                                   ! Initialize                                                 

    do                                             
        read(10,"(A)") line
        if(line(1:12) == "define") then
            exit
        end if
    end do

    do 
        read(line(21:40),*) bc_set_type                         ! Check the type of set of boundary condition
        
        if(bc_set_id > 10) then
            print*, "!!!!! ERROR : Boundary conditions exist more than 10 !!!!!"
            error stop
        else
            ! No problem
!!!!        end if

        ! Confirm the type of set is "node". This program accept displacement or POINT LOAD as a boundary conditions.
        if(bc_set_type == "node") then
            print *, "type of boundary conditions = node, OK"
        else
            print *, "!!!!! ERROR : THe type of boundary conditions are wrong !!!!! "
        end if
            
        read(line(61:80),*) bc_set_name(bc_set_id)               ! Store the name of set of boundary condition
            
        read(10,"(A)") line
        do i = 1, 1000 ! ループの回数 !
            read(line(20*i-9:20*i),*) bc_node_set(bc_set_id, i)  ! Store a list of node to apply boundary condition
        end do  ! このループ内は可変長に対応しないといけない

        bc_set_id = bc_set_id + 1

        read(10,"(A)") line
        if(line(1:12) == "define") then
            ! continuar
        else 
            exit
        end if
    end do
    


        
   

        read(10,"(A)") line ! これ要らなくね？
        ! Read fixed disp
        if(line(1:12) == "fixed disp") then
            read(10,*)

            do l = 1, 10

                do bc_set_id = 1, 10
                    read(10,"(A)") line
                    if(line(61:80) == bc_set_name(bc_set_id)) then            
                        read(10,"(A)") line

                        do i = 1, 3
                            read(line(20*i-19:20*i),*) field        ! Store a list of node to apply boundary condition
                            magnitude = expo2double(field)          ! Convert notation by expo2double
                            fixed_disp_magn(bc_set_id, i) = magnitude
                        end do  ! このループ内も可変長に対応しないといけない

                        read(10,*)
                        read(10,"(A)") line
                        do j = 1, 3
                            read(line(10*j-9:10*j),*) fixed_disp_vector(bc_set_id, j) ! Store a list of vecotr to give displacement
                        end do  ! このループ内も可変長に対応しないといけない
                    end if
                    read(10,*)
                    read(10,*)
                end do
            end do

        ! Read Dist load
        else if(line(1:12) == "point loads") then
            read(10,*)
            
            do l = 1, 10
                read(10,"(A)") line
                if(line(61:80) == bc_set_name(l)) then            
                    read(10,"(A)") line
                    do j = 1, 3
                        read(line(20*j-19:20*j),*) field            ! Store a list of node to apply boundary condition
                        fixed_disp_magnitude = expo2double(field)   ! Convert notation by expo2double
                    end do  ! このループ内も可変長に対応しないといけない
                end if
            end do

            

end subroutine read_geometry
subroutine read_dat(connect, coord, nnode, nelem, E, nu)
    use parameters
    implicit none

    !!!!! Get .dat file and extract model info and boundary conditions.

    ! Declear variable get from main
    integer :: nnode, nelem
    integer :: connect(nelem_max, 3)
    real(8) :: coord(nnode_max, 2)
    real(8) :: E, nu

    ! Declear local variable in read_dat
    character(len=256) :: inputfilename
    character(len=256) :: line, field
    integer :: ios_nelem, ios_nnode, elem_type, ios_elem_type
    integer :: i, j, l, m, n, pos, expo
    real(8) :: base
    

    !!! Specify .dat file to input
    call get_command_argument(1,inputfilename)                  ! Get the name of input file
    print *, "Input file = ",trim(inputfilename)
    open(10, file = trim(inputfilename), status="old", action="read")

    !!! Get geometry info of model
    ! Get model size
    do 
        read(10,"(A)") line
        if (line(1:10) == "sizing") then                        ! Buscar "sizing"
            read(line(41:50),*,iostat=ios_nelem) nelem          ! Store number of elements
            read(line(51:60),*,iostat=ios_nnode) nnode          ! Store number of nodes
            
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
        if(line(1:12) == "connectivity") then                   ! Buscar "connectivity"
            read(10,*)

            do m = 1, nelem
                read(10,"(A)") line
                read(line(16:20),*,iostat=ios_elem_type) elem_type
                    if(elem_type == 201) then                   ! Confirm type of elemnt == 201

                        read(line(6:10),*) i                    ! Store element number
                                                                ! The number of line "m" has possible to differ from element number "i".
                        read(line(26:30),*) connect(i, 1)       ! Store number of 1st node 
                        read(line(36:40),*) connect(i, 2)       ! Store number of 2nd node
                        read(line(46:50),*) connect(i, 3)       ! Store number of 3rd node

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
        if(line(1:11) == "coordinates") then                    ! Buscar "coordinates"
            read(10,*)
            do n = 1, nnode 
                read(10,"(A)") line
                read(line(6:10),*) j                            ! Store node number
                                                                ! The number of line "n" has possible to differ from node number "j".
                do l = 1, 2
                    read(line(20*l-9:20*l+10),*) field          ! Read value of node in each axis
                    coord(j, l) = expo2double(field)            ! Calcular value of node in each axis
                end do
            end do
            exit
        end if
    end do
             
    !!! Get boundary condition info
    !! これから書きます．

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
            E = expo2double(field)                              ! Convert notation by expo2double
            
            read(line(21:40),*) field
            nu = expo2double(field)                             ! Convert notation by expo2double
            exit
        end if
    end do
    
end subroutine read_dat
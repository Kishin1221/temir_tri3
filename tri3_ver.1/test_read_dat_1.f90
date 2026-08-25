!!! Module set 
module parameters
    implicit none
    integer, parameter :: nnode_max = 1000
    integer, parameter :: nelem_max = 2000
    integer, parameter :: nbc_max = 10

contains
    !!! This is the functions to convert values from marc_style_exponent natation to real(8) notation. 
    function expo2double(field) result(value)       
        character(len = 256), intent(in) :: field
        integer :: expo, pos
        real(8) :: base, value

        pos = scan(field, "+-", BACK=.true.)        ! Buscar sign
        read(field(:pos-1),*) base                  ! Extract base of value
        read(field(pos:),*) expo                    ! Extract exponent of value

        value =base * 10.0d0**expo                  ! Convert to double
    end function expo2double
end module parameters

!!! Main of the program 
program main
    use parameters
    implicit none

    !Declear variables for read_geometry
    integer :: datfile, nnode, nelem
    integer :: connect(nelem_max, 3)
    real(8) :: coord(nnode_max, 2)
    real(8) :: E, nu

    ! Declear variables for read_BC
    integer :: num_bc_set, bc_node_set(nbc_max,1000), fixed_disp_vector(nbc_max,3), num_node_in_set(nbc_max)
    real(8) :: fixed_disp_magn(nbc_max,3), point_load_magn(nbc_max,2)

    !Declear variable for make_D
    real(8) :: D(3,3)

    !Declear variables for make_B
    real(8) :: B(nelem_max,3,6)

    !Declear variables for this test_read_dat_1.f90
    integer :: i, j
    
    call read_geometry(datfile, connect, coord, nnode, nelem, E, nu)

    call read_BC(datfile, num_bc_set, bc_node_set, num_node_in_set, fixed_disp_vector, fixed_disp_magn, point_load_magn)

    call make_D(D, E, nu)
    
    call make_B(connect, coord, nelem, B)

    print *, "connectivity"
    do i = 1, nelem
        do j = 1, 3
            print *, connect(i, j)
        end do
        print *
    end do
    
    print *, "coordinates"
    do i = 1, nnode
        do j = 1, 2
            print *, coord(i, j)
        end do
        print *
    end do

    print *, "Boundary Conditions"
    do i = 1, num_bc_set
        print *, "The list of node to applied"
        print *, bc_node_set(i, 1:num_node_in_set(i))
        do j = 1, 2
            print *, "fixed_disp_magnitude"
            print *, fixed_disp_magn(i, j)
            print *, "fixed_disp_vector"
            print *, fixed_disp_vector(i, j)
            print *, "point load magnitude"
            print *, point_load_magn(i, j)
        end do
    end do

    print *, "E = "
    print *, E
    print *, "nu = "
    print *, nu

    print *, "D-matrix"
    do i=1, 3
        do j=1, 3
            print *, D(i, j)
        end do
        print *
    end do

    print *, "B-matrix"
    do i=1, nelem
        do j=1, 3
            print *, B(i,j,:)
        end do
        print *
    end do

    

end program main  
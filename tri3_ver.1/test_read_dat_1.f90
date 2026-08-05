module parameters
    implicit none
    integer, parameter :: nnode_max = 1000
    integer, parameter :: nelem_max = 2000

contains
    function expo2double(field) result(value)
        character(len = 256), intent(in) :: field
        integer :: expo, pos
        real(8) :: base, value

        pos = scan(field, "+-", BACK=.true.)        ! Buscar sign
        read(field(:pos-1),*) base                  ! Extract base of value
        read(field(pos:),*) expo                    ! Extract exponent of value

        value =base * 10.0d0**expo
    end function expo2double
end module parameters

program main
    use parameters
    implicit none

    ! Declear variable get from main
    integer :: nnode, nelem
    integer :: connect(nelem_max, 3)
    real(8) :: coord(nnode_max, 2)
    real(8) :: E, nu

    !Declear variable for make_D
    real(8) :: D(3,3)

    !Declear variable for make_B
    real(8) :: B(nelem_max,3,6)

    ! Declear variable in test_read_dat
    integer :: i, j
    
    call read_dat(connect, coord, nnode, nelem, E, nu)

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
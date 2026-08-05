module parameters
    implicit none
    integer, parameter :: nnode_max = 1000
    integer, parameter :: nelem_max = 2000

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

program main
    use parameters
    implicit none

    !!!!!!! "temir" is a FEM program for elastic homogeneous material. 
    
    !!!!! This program only accept tri3 element type.
    !!!!! Max size of model is defined below. 

    !Declear variable for read_dat
    integer :: nnode, nelem
    integer :: connect(nelem_max, 3)
    real(8) :: coord(nnode_max, 2)
    real(8) :: E, nu

    !Declear variable for make_D
    real(8) :: D(3,3)

    !Declear variable for make_B
    real(8) :: B(nelem_max,3,6)

    !Declear variable for assembly_K
    real(8) :: K(2*nnode_max, 2*nnode_max)

    !Dcelear variable for solver
    real(8) :: Force(2*nnode_max)
    real(8) :: disp(2*nnode_max)

    !Declear variable for strain_stress
    real(8) :: strain(nelem_max, 3)
    real(8) :: stress(nelem_max, 3)

    !!! Start Sbroutines    
    call read_dat(connect, coord, nnode, nelem, E, nu)

    call bc(connect, coord)

    call make_D(D, E, nu)

    call make_B(connect, coord, nelem, B)

    call assembly_K(connect, coord, K, D, B)

    call solver(connect, coord, nnode, nelem, D, B, K, Force, disp)

    call strain_stress(nelem, connect, D, B, disp, strain, stress)

    call write(connect, coord, strain, stress)

end program
!!!!!!! "temir" is a FEM program for elastic homogeneous material. !!!!!!!

!!!!! Program Specifications !!!!! 
! This version "temir_tri3" only accept marc element type 201. !!!!!
! Max size of model is specified in module. !!!!!
! Running command should be "~/path/temir_tri3_main.exe modeldatfil.dat". !

!!! To Avoid Error (The situations below are not expected in this program.) !!! 
! All boundary conditions defined in mentat must be used in job. !
! 

!!! Module set 
module parameters
    implicit none

    ! Specify the limit of model size
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
    integer :: datfile, nnode, nelem                ! datfile : input file, nnode : the number of nodes, nelem : the number of element
    integer :: connect(nelem_max, 3)                ! connect : connectivity of each element 
    real(8) :: coord(nnode_max, 2)                  ! coord : coordinate of each node
    real(8) :: E, nu, t                             ! E : Young modulous, nu = poisson ration, t = thickness of plane stress plate

    ! Declear variables for read_BC
    integer :: num_bc_set, bc_node_set(nbc_max,1000), fixed_disp_vector(nbc_max,3), num_node_in_set(nbc_max)
    real(8) :: fixed_disp_magn(nbc_max,3), point_load_magn(nbc_max,2)

    !Declear variable for make_D
    real(8) :: D(3,3)                               ! D : D-matrix 

    !Declear variables for make_B
    real(8) :: B(nelem_max,3,6), area(nelem_max)    ! B : B-matrix

    ! Declear variables for make_Ke
    real(8) :: Ke(nelem,6,6)

    !Declear variabls for assembly_K
    real(8) :: K(2*nnode_max, 2*nnode_max)

    !Dcelear variables for solver
    real(8) :: Force(2*nnode_max)
    real(8) :: disp(2*nnode_max)

    !Declear variables for strain_stress
    real(8) :: strain(nelem_max,3)
    real(8) :: stress(nelem_max,3)

    !!! Start Sbroutines    
    call read_geometry(datfile, connect, coord, nnode, nelem, E, nu)

    call read_BC(datfile, num_bc_set, bc_node_set, fixed_disp_vector, fixed_disp_magn, point_load_magn)

    call make_D(D, E, nu)

    call make_B(connect, coord, nelem, area, B)

    call make_Ke(nelem, D, B, area, t, Ke)

    call assemble_K(connect, coord, K, D, B)

    call solver(connect, coord, nnode, nelem, D, B, K, Force, disp)

    call strain_stress(nelem, connect, D, B, disp, strain, stress)

    call write(connect, coord, strain, stress)

end program
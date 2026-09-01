subroutine export_result(nnode, nelem, coord, disp, reaction_force, strain, stress)

    use parameters
    implicit none

    ! Input aarguments
    integer, intent(in) :: nnode, nelem
    real(8), intent(in) :: coord(nelem_max, 2) disp(2*nnode_max), reaction_force(2*nnode_max)
    real(8), intent(in) :: strain(nelem_max, 3), stress(nelem_max, 3)




end subroutine export_result
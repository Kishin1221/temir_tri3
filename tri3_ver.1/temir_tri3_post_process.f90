subroutine make_full_displacement()

    use parameters
    implicit none

    ! Declear variables get from main
    integer, intent(in) :: nnode, num_bc_set, bc_node_set(nbc_max,1000), num_node_in_set(nbc_max)
    integer, intent(in) :: shift_index(2*nnode_max), fixed_disp_vector(nbc_max,3)
    logical, intent(in) :: is_fixed(2*nnode_max)    
    real(8), intent(in) :: fixed_disp_magn(nbc_max,3), reduced_disp(2*nnode_max)

    ! Declear variables give to main
    real(8), intent(out) :: disp(2*nnode_max)

    ! Declear local variables in make_full_displacement
    integer :: i, j, k

    !!! Expand reduced_disp to disp
    disp = 0.0d0

    do i = 1, 2*nnode
        if (is_fixed(i) .eqv. .false.) then
            disp(i) = reduced_disp(i - shift_index(i))
        end if
    end do

    do k = 1, num_bc_set
        if (fixed_disp_magn(k, 1) /= 0.0d0) then
            do i = 1, num_node_in_set(k)
                disp(2 * bc_node_set(k, i) - 1) = fixed_disp_magn(k, 1)
            end do
        end if

        if (fixed_disp_magn(k, 2) /= 0.0d0) then
            do i = 1, num_node_in_set(k)
                disp(2 * bc_node_set(k, i)) = fixed_disp_magn(k, 2)
            end do
        end if
    end do

end subroutine make_full_displacement

subroutine reaction_force()

    use parameters
    implicit none

    ! Declear variables get from main
    integer, intent(in) :: nnode
    real(8), intent(in) ::

    ! Declear variables give to main
    real(8), intent(out) :: reaction_force(2*nnode_max)

end subroutine reaction_force

subroutine strain_stress()

    use parameters
    implicit none

    ! Declear varianles get from main
    integer, intent(in) :: nelem
    real(8), intent(in) :: D(3,3), B(nelem_max,3,6)
    real(8), intent(in) :: disp(2*nnode_max)

    ! Declear variables give to main
    real(8), intent(out) :: strain(nelem_max, 2), stress(nelem_max, 2)


end subroutine strain_stress




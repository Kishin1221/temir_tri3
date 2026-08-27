subroutine apply_BC(nnode, stiffness, num_node_set, num_node_in_set, bc_node_set, fixed_disp_vector, fixed_disp_magn, point_load_magn)

    use parameters
    implicit none

    !!!!! Get BC and global_stiffness matrix, and assemble in equations.

    ! Declear variables get from main
    integer, intent(in) :: nnode, bc_node_set(nbc_max,1000)
    integer, intent(in) :: fixed_disp_vector(nbc_max,3), num_node_in_set(nbc_max)
    real(8), intent(in) :: fixed_disp_magn(nbc_max,3), point_load_magn(nbc_max,2)
    real(8), intent(in) :: stiffness(2*nnode_max, 2*nnode_max)

    ! Declear variables give to main
    real(8), intent(out) :: reduced_stiffness(2*nnode_max,2*nnode_max), force(2*nnode_max)

    ! Declear local variables in apply_BC
    integer :: inode, num_fixed_dof_so_far, i, j, k
    logical :: is_fixed(2*nnode_max)


    ! Find fixed degrees of freedom 
    is_fixed = .false.                                      ! Initialize : assume that all degrees of freedom is not fixed
    do k = 1, num_bc_set
        do i = 1, 3
            if (fixed_disp_vector(k, i) == 1) then          ! When displacement of x is fixed
                do j = 1, num_node_in_set(k)
                    is_fixed(2 * bc_node_set(k, j) - 1) = .true.                ! .true. means this degree of freedom is fixed
                end do
                
            else if(fixed_disp_vector(k, i) == 2) then      ! When displacement of y is fixed
                do j = 1, num_node_in_set(k)
                    is_fixed(2 * bc_node_set(k, j)) = .true.                    ! .true. means this degree of freedom is fixed
                end do

            end if
        end do
    end do

    ! ここからやり直しだな．全くロジックがわからん．
    num_fixed_dof_so_far = 0
    do i = 1, 2*nnode
        do j = 1, 2*nnode
            if (is_fixed(i) == .false. .and. is_fixed(j) == .false.) then
                reduced_stiffness(i - num_fixed_dof_so_far, j - num_fixed_dof_so_far) = stiffness(i, j)
        
            else
                num_fixed_dof_so_far = num_fixed_dof_so_far + 1
            end if
        end do
    end do







    



end subroutine apply_BC
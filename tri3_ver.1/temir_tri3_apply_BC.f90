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
    integer :: shift_index(2*nnode_max)
    logical :: is_fixed(2*nnode_max)


    ! Find fixed degrees of freedom 
    ! The index of is_fixed is corresponding to degrees of freedom of all nodes.
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

    ! Count the number of DOF fixed by BC fixed_disp so far
    ! shift_index is defined to original index, not to after reductions. 英語が怪しいな
    shift_index = 0                                         ! Initialize
    do i = 1, 2*nnode
        if (is_fixed(i) == .true.) then
            if (i == 1) then
                shift_index(i) = 1                          ! When i = 1, shift_index(i-1) does not exist.
            
            else 
                shift_index(i) = shift_index(i-1) + 1       ! shift_index incrase
        
            end if
        
        else
            if (i == 1) then
                shift_index(i) = 0                          ! When i = 1, shift_index(i-1) does not exist.

            else
                shift_index(i) = shift_index(i-1)
            
            end if

        end if
    end do

    ! Reduction of stiffness matrix
    reduced_stiffness = 0.0d0                               ! Initialize
    do i = 1, 2*nnode
        do j = 1, 2* nnode
            if (is_fixed(i) == .false. .and. is_fixed(j) == .false.) then                           ! If both of row and colmun are not fixed
                reduced_stiffness(i - shift_index(i), j - shift_index(j)) = stiffness(i, j)         ! Reduction. Store the entry of stiffness matrix
            
            else
                ! Do not store

            end if
        end do
    end do

    ! Make force vector corresponding to reduced stiffness matrix.
    reduced_force = 0.0d0
    do k = 1, num_bc_set
        if (point_load_magn(k, 1) /= 0.0d0) then            ! If the value of load for x-axis is not 0 
            do i = 1, num_node_in_set(k)
                if (is_fixed(2 * bc_node_set(k, i) -1) == .false. ) then
                    reduced_force(2 * bc_node_set(k, i) - 1 - shift_index(2 * bc_node_set(k, i) -1)) = point_load_magn(k, 1)
                end if
            end do
        end if

        if (point_load_magn(k, 2) /= 0.0d0) then            ! If the value of load for y-axis is not 0
            do i = 1, num_node_in_set(k)
                if (is_fixed(2 * bc_node_set(k, i)) == .false.) then
                    reduced_force(2 * bc_node_set(k, i) - shift_index(2 * bc_node_set(k, i))) = point_load_magn(k, 2)
                end if
            end do
                                                            ! If the value of load is 0, 
        end if
    end do

    ! 混乱．ゆっくりやり直そう．
    ! 変位境界条件が非0の値である場合の移項
    do k = 1, num_bc_set
        if (fixed_disp_magn(k, 1) /= 0.0d0) then            ! サイズは3列用意してあるが，平面なので3列目は使わない．
                do i = 1, num_node_in_set(k)
                    trans_dof(i) = 2 * bc_node_set(k, i) - 1 -shift_index(2 * bc_node_set(k, i) - 1)
                    reduced_force(trans_dof) = reduced_force(trans_dof) - reduced_stiffness(, trans_dof) * fixed_disp_magn(k, 1)





end subroutine apply_BC
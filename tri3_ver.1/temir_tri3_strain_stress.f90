subroutine strain_stress(nelem, connect, D, B, disp, strain, stress)
    use parameters
    implicit none

    !!!!! Get displacement and D and B matrix, then calculate strain and stress.

    ! Declear variable get from main
    integer, intent(in) :: nelem
    integer, intent(in) :: connect(nelem_max, 3)
    real(8), intent(in) :: D(3,3)
    real(8), intent(in) :: B(nelem_max,3,6)
    real(8), intent(in) :: disp(2*nnode_max, 1)
    real(8), intent(out) :: strain(nelem_max, 3)
    real(8), intent(out) :: stress(nelem_max, 3)

    ! Declear local variable in strain_stress
    integer :: e,i,j,k,l
    real(8) :: disp_local(6)

    !!! Vamos a Calcular
    do e = 1, nelem

        !Extract displacement of element No.e 
        do i=1,3
            disp_local(2*i-1) = disp(2*connect(e, i)-1)
            disp_local(2*i) = disp(2*connect(e, i))
        end do
        
        !Reset strain and stress
        do j = 1, 3
            strain(e, j) = 0.0d0
            stress(e, j) = 0.0d0
        end do

        !Calculate strain
        do j = 1, 3
            do k = 1, 6
                strain(e, j) = strain(e, j) + B(e, j, k) * disp_local(k)
            end do
        end do

        !Calculate stress
        do j = 1,3
            do l = 1, 3
                stress(e, j) = stress(e, j) + D(j, l) * strain(e, l)
            end do
        end do
    
    end do

end subroutine strain_stress
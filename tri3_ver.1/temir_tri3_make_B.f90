subroutine make_B(connect, coord, nelem, B)
    use parameters
    implicit none

    !!!!! Get model info and make B-matrix

    !Declear variable get from main
    integer :: nelem
    integer, intent(in) :: connect(nelem_max, 3)
    real(8), intent(in) :: coord(nnode_max, 2)
    real(8), intent(out) :: B(nelem_max, 3, 6)

    !Declear local variable in make_B 
    integer :: e,i,m,n
    real(8) :: area
    real(8) :: bi(nelem_max,3), ci(nelem_max,3)

    !!! Calculate entries of B-matrix
    do e = 1, nelem
        
        !Reset to 0
        do m = 1, 3
            do n = 1, 6
                B (e, m, n) = 0.0d0
            end do
        end do

        area = 0.0d0

        bi(e,1) = coord(connect(e, 2),2) - coord(connect(e, 3),2)
        bi(e,2) = coord(connect(e, 3),2) - coord(connect(e, 1),2)
        bi(e,3) = coord(connect(e, 1),2) - coord(connect(e, 2),2)
        ci(e,1) = coord(connect(e, 3),1) - coord(connect(e, 2),1)
        ci(e,2) = coord(connect(e, 1),1) - coord(connect(e, 3),1)
        ci(e,3) = coord(connect(e, 2),1) - coord(connect(e, 1),1)  

        do i = 1,3
            area = area + (coord(connect(e, i),1) * bi(e, i))/2
        end do
        
        !Caluculate entry of B-matrix
        B(e, 1, 1) = bi(e, 1)/(2*area)
        B(e, 1, 3) = bi(e, 2)/(2*area)
        B(e, 1, 5) = bi(e, 3)/(2*area)
        B(e, 2, 2) = ci(e, 1)/(2*area)
        B(e, 2, 4) = ci(e, 2)/(2*area)
        B(e, 2, 6) = ci(e, 3)/(2*area)
        B(e, 3, 1) = B(e, 2, 2) 
        B(e, 3, 2) = B(e, 1, 1) 
        B(e, 3, 3) = B(e, 2, 4) 
        B(e, 3, 4) = B(e, 1, 3) 
        B(e, 3, 5) = B(e, 2, 6) 
        B(e, 3, 6) = B(e, 1, 5) 

    end do
    
end subroutine make_B
C     $Id: vprod.f 5093 2015-06-10 13:48:23Z brideout $
C
      SUBROUTINE VPROD(A,B,C)
C
C     JMH - 11/79  ANS FORTRAN 66
C
C     VPROD CALCULATES THE VECTOR PRODUCT OF TWO VECTORS A AND B,
C     C = A .CROSS. B.
C     $Id: vprod.f 5093 2015-06-10 13:48:23Z brideout $
C
C     .. Array Arguments ..
      DOUBLE PRECISION A(3),B(3),C(3)
C     ..
      C(1) = A(2)*B(3) - A(3)*B(2)
      C(2) = A(3)*B(1) - A(1)*B(3)
      C(3) = A(1)*B(2) - A(2)*B(1)
      RETURN
C
      END

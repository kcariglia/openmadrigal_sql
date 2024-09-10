C     $Id: tprod.f 5093 2015-06-10 13:48:23Z brideout $
C
      DOUBLE PRECISION FUNCTION TPROD(A,B,C)
C
C     JMH - 11/79  ANS FORTRAN 66
C
C     TPROD CALCULATES THE TRIPLE PRODUCT OF THREE VECTORS A, B AND C.
C     TPROD = A .DOT. (B .CROSS. C).
C     $Id: tprod.f 5093 2015-06-10 13:48:23Z brideout $
C
C     .. Array Arguments ..
      DOUBLE PRECISION A(3),B(3),C(3)
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION D(3)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPROD
      EXTERNAL SPROD
C     ..
C     .. External Subroutines ..
      EXTERNAL VPROD
C     ..
      CALL VPROD(B,C,D)
      TPROD = SPROD(A,D)
      RETURN
C
      END

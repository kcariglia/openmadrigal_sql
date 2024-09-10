C     $Id: vrecip.f 5093 2015-06-10 13:48:23Z brideout $
C
      SUBROUTINE VRECIP(A,B,C,APR,BPR,CPR)
C
C     JMH - 11/79  ANS FORTRAN 66
C
C     VRECIP CALCULATES THE RECIPROCAL VECTORS APR, BPR, CPR
C     CORRESPONDING TO THREE LINEARLY INDEPENDENT VECTORS A, B, C.
C     $Id: vrecip.f 5093 2015-06-10 13:48:23Z brideout $
C
C     .. Array Arguments ..
      DOUBLE PRECISION A(3),APR(3),B(3),BPR(3),C(3),CPR(3)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION T
      INTEGER I
C     ..
C     .. External Functions ..
      DOUBLE PRECISION TPROD
      EXTERNAL TPROD
C     ..
C     .. External Subroutines ..
      EXTERNAL VPROD
C     ..
      T = TPROD(A,B,C)
      CALL VPROD(B,C,APR)
      CALL VPROD(C,A,BPR)
      CALL VPROD(A,B,CPR)
      DO 10 I = 1,3
         APR(I) = APR(I)/T
         BPR(I) = BPR(I)/T
         CPR(I) = CPR(I)/T
   10 CONTINUE
      RETURN
C
      END

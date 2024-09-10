C     $Id: uvect.f 5093 2015-06-10 13:48:23Z brideout $
C
      SUBROUTINE UVECT(A,B)
C
C     JMH - 11/79  ANS FORTRAN 66
C
C     VPROD NORMALIZES VECTOR A TO UNIT VECTOR B.
C     $Id: uvect.f 5093 2015-06-10 13:48:23Z brideout $
C
C     .. Array Arguments ..
      DOUBLE PRECISION A(3),B(3)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AMAG
C     ..
C     .. External Functions ..
      DOUBLE PRECISION VMAG
      EXTERNAL VMAG
C     ..
      AMAG = VMAG(A)
      B(1) = A(1)/AMAG
      B(2) = A(2)/AMAG
      B(3) = A(3)/AMAG
      RETURN
C
      END

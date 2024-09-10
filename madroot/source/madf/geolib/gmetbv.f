C     $Id: gmetbv.f 5093 2015-06-10 13:48:23Z brideout $
C
      SUBROUTINE GMETBV(E1,E2,E3,G)
C
C     jmh - 5/06  ans fortran 66
C
C     GMETBV calculates the metric tensor G of a coordinate system given
C     base vecors e1,e2,e3. See Margenau and Murphy, 5-86.
C
C       Input:
C         E1,E2,E3 - base vectors
C
C      Output:
C         G - metric tensor.
C     $Id: gmetbv.f 5093 2015-06-10 13:48:23Z brideout $
C
C     .. Array Arguments ..
      DOUBLE PRECISION E1(3),E2(3),E3(3),G(3,3)
C     ..
C     .. Local Scalars ..
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPROD
      EXTERNAL SPROD
C     ..
      G(1,1) = SPROD(E1,E1)
      G(2,1) = SPROD(E2,E1)
      G(3,1) = SPROD(E3,E1)
      G(1,2) = SPROD(E1,E2)
      G(2,2) = SPROD(E2,E2)
      G(3,2) = SPROD(E3,E2)
      G(1,3) = SPROD(E1,E3)
      G(2,3) = SPROD(E2,E3)
      G(3,3) = SPROD(E3,E3)
      RETURN
C
      END

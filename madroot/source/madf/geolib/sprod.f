C     $Id: sprod.f 3304 2011-01-17 15:25:59Z brideout $
C
      DOUBLE PRECISION FUNCTION SPROD(A,B)
C
C     jmh - 11/79  ans fortran 66
C
C     SPROD calculates the scalar product of two vectors A and B.
C
C     Input:
C        A - floating point vector of dimension 3
C        B - floating point vector of dimension 3
C
C     .. Array Arguments ..
      DOUBLE PRECISION A(3),B(3)
C     ..
      SPROD = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
      RETURN
C
      END

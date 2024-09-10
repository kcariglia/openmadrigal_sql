C     $Id: vmag.f 3304 2011-01-17 15:25:59Z brideout $
C
      DOUBLE PRECISION FUNCTION VMAG(A)
C
C     jmh - 1/80  ans fortran 66
C
C     VMAG calculates the magnitude of a vector A
C
C     Input:
C        A - floating point vector of dimension 3
C
C     .. Array Arguments ..
      DOUBLE PRECISION A(3)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DSQRT
C     ..
      VMAG = DSQRT(A(1)*A(1)+A(2)*A(2)+A(3)*A(3))
      RETURN
C
      END

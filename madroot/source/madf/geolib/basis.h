C     Written by Shunrong Zhang
C     Imported into Madrigal from modelRecovery/fullAnalytic/
C     basis.h on 10/21/2005 - Was revision 1.1.1.1
C
C     $Id: basis.h 3304 2011-01-17 15:25:59Z brideout $
C
C     .. Parameters ..
C
      INTEGER MDIM,NDIM,KMAX,KNOTSMAX
      PARAMETER (MDIM=8000,NDIM=4000,KMAX=4,KNOTSMAX=NDIM+KMAX,
     * LA = NDIM*MDIM+4*NDIM+5*MDIM+NDIM**2+MDIM*(MDIM+3)/2+4*MDIM,
     * LI = 2*NDIM*MDIM)
C
      COMMON /ARRAYS/IWORK,WORK
      DOUBLE PRECISION WORK(9000000)
      INTEGER IWORK(900000)
c      DOUBLE PRECISION WORK(LA)
c      INTEGER IWORK(LI)

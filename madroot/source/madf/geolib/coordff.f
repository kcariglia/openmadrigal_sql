C     $Id: coordff.f 7598 2023-08-25 13:50:23Z brideout $
C
      SUBROUTINE COORDFF(SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,
     *    GDLAT,GLON,GDALT,QDIAG,QSPHERICAL,RCOR)
C     Calculates the listed coordinates of a specified point. the point
C     may be specified either by slatgd, slon, sr, slatgc, tm, az, el,
C     range (range .ge. 0.) or by gdlat, glon, gdalt, tm (range .lt.
C     0.). Most of the output parameters involve the geomagnetic field
C     as specified by the International Geomagnetic Referenc eField
C     (IGRF) fo the specified epoch (tm).  Apex latitude and longitude,
C     along with either gdalt or arc form a geomagnetic coordinate
C     system within either hemisphere. This system is necessarily
C     non-orthogonal which accounts for the complexity of many of the
C     calculations in coord. Apex latitude is always positive which is
C     appropriate when using apex latitude and longitude to label
C     field lines.
C
C     This is an even faster version of coord.f used by getMag and faraday
C     because coord is slower calculating parms not needed for
C     those methods. RCOR(11-64) always return 0.
C
C     Input:
C        SLATGD - station geodetic latitude
C        SLON   - station longitude
C        SR     - radial distance of station from center of earth
C        SLATGC - station geocentric latitude
C        TM     - time in years (e.g. 1975.2)
C        AZ     - radar azimuth
C        EL     - radar elevation
C        RANGE  - range to observation point
C        QDIAG - Print diagnostic output on terminal if non-zero
C        QSPHERICAL - Assume spherical coordinates instead of apex coordinates 
C                     if non-zero. This helps verify the numerical 
C                     nonorthogonal coordinate calculations because the 
C                     spherical case also  has an analytic solution.
C        QDIAG and QSPHERICAL should be set to 0 except for development
C     Input, output:
C        GDLAT  - geodetic latitude of observation point
C        GLON   - longitude of observation point
C        GDALT  - altitude above spheroid of observation point
C     Output:
C        RCOR(1)  - az    - radar azimuth
C        RCOR(2)  - el    - radar elevation
C        RCOR(3)  - range - range to observation point
C        RCOR(4)  - gdlat - geodetic latitude of observation point
C        RCOR(5)  - glon  - longitude of observation point
C        RCOR(6)  - gdalt - altitude above spheroid of observation point
C        RCOR(7)  - b     - magnitude of geomagnetic field
C        RCOR(8)  - br    - radial component of geomagnetic field
C        RCOR(9)  - bt    - southward component of geomagnetic field
C        RCOR(10) - bp    - eastward component of geomagnetic field
C        RCOR(11) - rlatm - dip latitude
C        RCOR(12) - rlati - invariant latitude
C        RCOR(13) - rl    - magnetic l parameter
C        RCOR(14) - alat  - apex latitude
C        RCOR(15) - alon  - apex longitude
C
C        RCOR(16) - g(1,1) magnetic coordinate system metric tensor,
C                          upper half stored row-wise
C        RCOR(17) - g(2,1) "                                       "
C        RCOR(18) - g(2,1) "                                       "
C        RCOR(19) - g(2,1) "                                       "
C
C        RCOR(20) - ctab(1) - direction cosine of beam wrt geodetic
C                             south
C        RCOR(21) - ctab(2) - direction cosine of beam wrt geodetic
C                             east
C        RCOR(22) - ctab(3) - direction cosine of beam wrt geodetic
C                             altitude (up)
C
C        RCOR(23) - ctab(4) - direction cosine of beam wrt apex
C                             latitude (northward)
C        RCOR(24) - ctab(5) - direction cosine of beam wrt apex
C                             longitude (eastward)
C        RCOR(25) - ctab(6) - direction cosine of beam wrt fieldline
C                             geomagnetic field (up)
C
C        RCOR(26) - cost1 - x-direction cosine of a vector
C                           perpendicularto l.o.s. w/respect to apex
C                           coords.
C        RCOR(27) - cost2 - y-direction cosine "                   "
C        RCOR(28) - cost3 - z-direction cosine "                   "
C
C        RCOR(29) - ainc - inclination of geomagnetic field
C        RCOR(30) - dec - declination of geomagnetic field
C        RCOR(31) - gclat - geocentric latitude
C        RCOR(32) - aspct - aspect angle
C        RCOR(33) - cplat - conjugate geocentric latitude
C        RCOR(34) - gcdlat - conjugate geodetic latitude
C        RCOR(35) - cplon conjugate longitude
C        RCOR(36 - 64) are set to zero.  Use coord.f for those
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION AZ,EL,GDALT,GDLAT,GLON,RANGE,SLATGC,SLATGD,SLON,
     *                 SR,TM
      INTEGER QDIAG,QSPHERICAL
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION RCOR(64)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AINC,ALAT,ALON,ARAD,ARC,ASPCT,B,BB,BP,BPERPEASTM,
     *                 BPERPSOUTHM,BR,BT,BX,BY,BZ,CALAT,CALON,CARAD,
     *                 CARC,CASPCT,CGDALT,CGDLAT,
     *                 COST1,COST2,COST3,CP,CPLAT,
     *                 CPLON,CPRKM,CSP,CSR,CST,CT,CX,CY,CZ,D,DEC,
     *                 DECGC,DTR,
     *                 EASTM,EASTP,EASTR,EASTT,EASTX,
     *                 EASTY,EASTZ,
     *                 EFMAG,EFX,EFY,EFZ,
     *                 GCALT,GCLAT,GDLATS,
     *                 HB,HI,P,PERPEASTP,PERPEASTR,PERPEASTT,PERPSOUTHP,
     *                 PERPSOUTHR,PERPSOUTHT,PFX,PFY,PFZ,PLAT,PLON,
     *                 PRKM,PX,PY,PZ,RFP,RFR,RFT,RFX,RFY,RFZ,
     *                 RKM,RL,RLATI,RLATM,RP,RR,RT,SOUTHM,SOUTHP,
     *                 SOUTHR,SOUTHT,SOUTHX,SOUTHY,SOUTHZ,SP,ST,T,
     *                 TM1,UPBP,UPBR,UPBT,UPP,UPR,UPT,UPX,UPY,
     *                 UPZ,XB,XDUM,YB,ZB
      INTEGER IER,INIT,ISTOP,NPR
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION BF(3),BPERPEAST(3),BPERPSOUTH(3),CTAB(9),
     *                 EAST(3),EAST2(3),
     *                 EF(3),
     *                 PERPEAST(3),PERPEAST2(3),
     *                 PERPSOUTH(3),PERPSOUTH2(3),PF(3),Q0(3),
     *                 RF(3),SOUTH(3),SOUTH2(3),UP(3),
     *                 UP2(3),UPB(3),UPB2(3),ZEROM(3)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPROD,TPROD,VMAG
      EXTERNAL SPROD,TPROD,VMAG
C     ..
C     .. External Subroutines ..
      EXTERNAL CONVRT,CSCONV,GDV,GMET,GMETBV,INVAR,LINTRA,LOOK,MILMAG,
     *         MINV,MTRAN3,POINT,RPCART,UVECT,VCTCNV,VPROD,VRECIP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ACOS,ATAN,COS,DATAN2,DBLE,DSQRT,MAX,MIN,
     *          SIN,SQRT
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION TAN
C     ..
C     .. Equivalences ..
      EQUIVALENCE (RFX,RF(1)),(RFY,RF(2)),(RFZ,RF(3))
      EQUIVALENCE (PFX,PF(1)),(PFY,PF(2)),(PFZ,PF(3))
      EQUIVALENCE (BX,BF(1)),(BY,BF(2)),(BZ,BF(3))
      EQUIVALENCE (EFX,EF(1)),(EFY,EF(2)),(EFZ,EF(3))
      EQUIVALENCE (SOUTHX,SOUTH(1)),(SOUTHY,SOUTH(2)),(SOUTHZ,SOUTH(3))
      EQUIVALENCE (EASTX,EAST(1)),(EASTY,EAST(2)),(EASTZ,EAST(3))
      EQUIVALENCE (UPX,UP(1)),(UPY,UP(2)),(UPZ,UP(3))
C     ..
C     .. Data statements ..
      DATA DTR/0.0174532925199D0/
C     ..
C     .. Constants
C     err1 and err2 are error flags from geo-cgm.f
C     .. Statement Function definitions ..
      TAN(XDUM) = DSIN(XDUM)/(DCOS(XDUM)+1.D-38)
C     ..
C     .....Initialize.....
      COST1 = 0.0D0
      COST2 = 0.0D0
      COST3 = 0.0D0
      XB = 0.0D0
      YB = 0.0D0
      ZB = 0.0D0
      TM1 = ABS(TM)
      IF (RANGE.LT.0.0D0) THEN
C
C        .....Entry when gdlat,glon,gdalt are given.....
C        .....Calculation fails when gdlat is too close to 0 or 90.....
         GDLATS = GDLAT
         GDLAT = MIN(GDLAT,89.99D0)
         GDLAT = MAX(GDLAT,-89.99D0)
         IF (ABS(GDLAT).LT.0.01D0) THEN
            GDLAT = 0.01D0
         END IF
         CALL CONVRT(1,GDLAT,GDALT,GCLAT,RKM)
         CALL LOOK(SR,SLATGC,SLON,RKM,GCLAT,GLON,AZ,EL,RANGE)
      ELSE
C
C        .....Entry when az, el, range are given.....
         CALL POINT(SR,SLATGC,SLON,AZ,EL,RANGE,RKM,GCLAT,GLON)
         CALL CONVRT(2,GDLAT,GDALT,GCLAT,RKM)
         GDLATS = GDLAT
         GDLAT = MIN(GDLAT,89.99D0)
         GDLAT = MAX(GDLAT,-89.99D0)
         IF (ABS(GDLAT).LT.0.01D0) THEN
            GDLAT = 0.01D0
         END IF
      END IF
C
C     .....We now have all needed information on station, radar beam
C          and location of measurement point.....
C
      IF (QSPHERICAL .NE. 0) THEN
         SLATGD = SLATGC
         GDLAT = GCLAT
         GDALT = RKM - SR
      END IF
C
      IF (QDIAG .NE. 0) THEN
         WRITE (6,'(''Locations of radar and measurement:'')')
         WRITE (6,FMT='(''slatgd,slon,sr,slatgc,tm,az,el,range = '')')
         WRITE (6,FMT='(8f9.2)') SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE
         WRITE (6,FMT='(''gclat,glon,rkm = '')')
         WRITE (6,FMT='(3f9.2)') GCLAT,GLON,RKM
         WRITE (6,FMT='(''gdlat,glon,gdalt = '')')
         WRITE (6,FMT='(3f9.2)') GDLAT,GLON,GDALT
         WRITE (6,FMT='('' '')')
      END IF
C
C     .....Calculate magnetic field at observation point.....
      T = DTR*(90.D0-GCLAT)
      CT = DCOS(T)
      ST = DSIN(T)
      P = DTR*GLON
      CP = DCOS(P)
      SP = DSIN(P)
      CALL MILMAG(TM1,RKM,ST,CT,SP,CP,BR,BT,BP,B)
      IF (QSPHERICAL .NE. 0) THEN
         BR = -1.0D0
         BT = 0.0D0
         BP = 0.0D0
      END IF
      CALL GDV(GDLAT,GCLAT,BR,BT,BP,XB,YB,ZB)
C
C
C     .....Fill rcor with calculated coordinates.....
      GDLAT = GDLATS
      RCOR(1) = AZ
      RCOR(2) = EL
      RCOR(3) = RANGE
      RCOR(4) = GDLAT
      RCOR(5) = GLON
      RCOR(6) = GDALT
      RCOR(7) = B
      RCOR(8) = BR
      RCOR(9) = BT
      RCOR(10) = BP
      RCOR(11) = 0.0
      RCOR(12) = 0.0
      RCOR(13) = 0.0
      RCOR(14) = 0.0
      RCOR(15) = 0.0
      RCOR(16) = 0.0
      RCOR(17) = 0.0
      RCOR(18) = 0.0
      RCOR(19) = 0.0
      RCOR(20) = 0.0
      RCOR(21) = 0.0
      RCOR(22) = 0.0
      RCOR(23) = 0.0
      RCOR(24) = 0.0
      RCOR(25) = 0.0
      RCOR(26) = 0.0
      RCOR(27) = 0.0
      RCOR(28) = 0.0
      RCOR(29) = 0.0
      RCOR(30) = 0.0
      RCOR(31) = 0.0
      RCOR(32) = 0.0
      RCOR(33) = 0.0
      RCOR(34) = 0.0
      RCOR(35) = 0.0
      RCOR(36) = 0.0
      RCOR(37) = 0.0
      RCOR(38) = 0.0
      RCOR(39) = 0.0
      RCOR(40) = 0.0
      RCOR(41) = 0.0
      RCOR(42) = 0.0
      RCOR(43) = 0.0
      RCOR(44) = 0.0
      RCOR(45) = 0.0
      RCOR(46) = 0.0
      RCOR(47) = 0.0
      RCOR(45) = 0.0
      RCOR(46) = 0.0
      RCOR(47) = 0.0
      RCOR(48) = 0.0
      RCOR(49) = 0.0
      RCOR(50) = 0.0
      RCOR(51) = 0.0
      RCOR(52) = 0.0
      RCOR(53) = 0.0
      RCOR(54) = 0.0
C
      RETURN
      END

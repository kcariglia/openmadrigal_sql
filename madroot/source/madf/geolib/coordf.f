C     $Id: coordf.f 7596 2023-08-24 19:55:02Z brideout $
C
      SUBROUTINE COORDF(SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,GDLAT,GLON,
     *                 GDALT,QDIAG,QSPHERICAL,RCOR)
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
C     This is a faster version of coord.f used by getMag and faraday
C     because coord is slower calculating parms not needed for
C     those methods. RCOR(36-64) always return 0.
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
      IF ((QDIAG .NE. 0) ) THEN
         WRITE (6,FMT='(''b = '', 9F9.5)') BR,BT,BP,XB,YB,ZB,BX,BY,BZ
      END IF
C
C     .....Calculate inclination.....
      HB = DSQRT(XB*XB+YB*YB)
      AINC = DATAN2(ZB,HB)/DTR
C
C     .....Calculate declination.....
      DEC = DATAN2(YB,XB)/DTR
C
C     .....Convert southward, eastward, outward components of magnetic
C          field at observation point to earth centered cartesian
C          coordinates.....
      CALL VCTCNV(BX,BY,BZ,PX,PY,PZ,BR,BT,BP,RKM,90.D0-GCLAT,GLON,2)
C
C     .....Compute unit vectors perpendicular to magnetic field and
C          southward (bperpsouth) and eastward (bperpeast)
      DECGC = DATAN2(BP,-BT)/DTR
      IF (QSPHERICAL .NE. 0) THEN
         AINC = 90.0D0
         DECGC = 0.0D0
      END IF
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''ainc,dec,decgc = '', 3f10.5)') AINC,DEC,DECGC
      END IF
      SOUTHR = 0.0D0
      SOUTHT = COS(DTR*DECGC)
      SOUTHP = -SIN(DTR*DECGC)
      EASTR = 0.0D0
      EASTT = SIN(DTR*DECGC)
      EASTP = COS(DTR*DECGC)
      UPR = 1.0D0
      UPT = 0.0D0
      UPP = 0.0D0
      CALL VCTCNV(SOUTHX,SOUTHY,SOUTHZ,PX,PY,PZ,SOUTHR,SOUTHT,SOUTHP,
     *            RKM,90.D0-GCLAT,GLON,2)
      CALL VCTCNV(EASTX,EASTY,EASTZ,PX,PY,PZ,EASTR,EASTT,EASTP,RKM,
     *            90.D0-GCLAT,GLON,2)
      CALL VCTCNV(UPX,UPY,UPZ,PX,PY,PZ,UPR,UPT,UPP,RKM,90.D0-GCLAT,GLON,
     *            2)
      SOUTHM = VMAG(SOUTH)
      SOUTHX = SOUTHX/SOUTHM
      SOUTHY = SOUTHY/SOUTHM
      SOUTHZ = SOUTHZ/SOUTHM
      EASTM = VMAG(EAST)
      EASTX = EASTX/EASTM
      EASTY = EASTY/EASTM
      EASTZ = EASTZ/EASTM
      CALL VPROD(BF,EAST,BPERPSOUTH)
      BPERPSOUTHM = VMAG(BPERPSOUTH)
      PERPSOUTH(1) = BPERPSOUTH(1)/BPERPSOUTHM
      PERPSOUTH(2) = BPERPSOUTH(2)/BPERPSOUTHM
      PERPSOUTH(3) = BPERPSOUTH(3)/BPERPSOUTHM
      CALL VPROD(SOUTH,BF,BPERPEAST)
      BPERPEASTM = VMAG(BPERPEAST)
      PERPEAST(1) = BPERPEAST(1)/BPERPEASTM
      PERPEAST(2) = BPERPEAST(2)/BPERPEASTM
      PERPEAST(3) = BPERPEAST(3)/BPERPEASTM
      UPB(1) = -BF(1)/VMAG(BF)
      UPB(2) = -BF(2)/VMAG(BF)
      UPB(3) = -BF(3)/VMAG(BF)
C
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''southr,southt,southp,eastr,eastt,eastp = '')')
         WRITE (6,FMT='(3e13.5,3x,3e13.5)') SOUTHR,SOUTHT,SOUTHP,EASTR,
     *     EASTT,EASTP
         WRITE (6,FMT='(''south = '', 4e13.5)') SOUTH,SPROD(SOUTH,SOUTH)
         WRITE (6,FMT='('' east = '', 4e13.5)') EAST,SPROD(EAST,EAST)
         WRITE (6,FMT='(''   up = '', 4e13.5)') UP,SPROD(UP,UP)
         SP = SPROD(SOUTH,EAST)
         WRITE (6,FMT='(''sp = '', e13.5)') SP
         CALL VPROD(SOUTH,EAST,ZEROM)
         WRITE (6,FMT='(''zerom = '', 4e13.5)') ZEROM,SPROD(ZEROM,ZEROM)
         CALL VPROD(UP,SOUTH,EAST2)
         CALL VPROD(EAST,UP,SOUTH2)
         CALL VPROD(SOUTH,EAST,UP2)
         WRITE (6,FMT='(''south  = '', 3e13.5)') SOUTH
         WRITE (6,FMT='(''south2 = '', 3e13.5)') SOUTH2
         WRITE (6,FMT='(''east   = '', 3e13.5)') EAST
         WRITE (6,FMT='(''east2  = '', 3e13.5)') EAST2
         WRITE (6,FMT='(''up     = '', 3e13.5)') UP
         WRITE (6,FMT='(''up2  =   '', 3e13.5)') UP2
         WRITE (6,FMT='(''bperpsouth,bperpsouthm,perpsouth = '')')
         WRITE (6,FMT='(7e13.5)') BPERPSOUTH,BPERPSOUTHM,PERPSOUTH
         WRITE (6,FMT='(''bperpeast,bperpeastm,perpeast = '')')
         WRITE (6,FMT='(7e13.5)') BPERPEAST,BPERPEASTM,PERPEAST
         WRITE (6,FMT='(''bf,bfm,upb = '', 7e13.5)') BF,VMAG(BF),UPB
         WRITE (6,FMT='(''perpsouth,perpeast,upb = '')')
         WRITE (6,FMT='(2f8.2,3f8.4,2x,3f8.4,2x,3f8.4)') GCLAT,GLON,
     *     PERPSOUTH,PERPEAST,UPB
         CALL VCTCNV(PERPSOUTH(1),PERPSOUTH(2),PERPSOUTH(3),PX,PY,PZ,
     *               PERPSOUTHR,PERPSOUTHT,PERPSOUTHP,RKM,90.0D0-GCLAT,
     *               GLON,1)
         CALL VCTCNV(PERPEAST(1),PERPEAST(2),PERPEAST(3),PX,PY,PZ,
     *               PERPEASTR,PERPEASTT,PERPEASTP,RKM,90.0D0-GCLAT,
     *               GLON,1)
         CALL VCTCNV(UPB(1),UPB(2),UPB(3),PX,PY,PZ,UPBR,UPBT,UPBP,RKM,
     *               90.0D0-GCLAT,GLON,1)
         WRITE (6,FMT='(3f8.4,2x,3f8.4,2x,3f8.4)') PERPSOUTHR,
     *     PERPSOUTHT,PERPSOUTHP
         WRITE (6,FMT='(3f8.4,2x,3f8.4,2x,3f8.4)') PERPEASTR,PERPEASTT,
     *     PERPEASTP
         WRITE (6,FMT='(3f8.4,2x,3f8.4,2x,3f8.4)') UPBR,UPBT,UPBP
         CALL VPROD(UPB,PERPSOUTH,PERPEAST2)
         WRITE (6,FMT='(''perpeast   = '', 3e13.5)') PERPEAST
         WRITE (6,FMT='(''perpeast2  = '', 3e13.5)') PERPEAST2
         CALL VPROD(PERPEAST,UPB,PERPSOUTH2)
         WRITE (6,FMT='(''perpsouth  = '', 3e13.5)') PERPSOUTH
         WRITE (6,FMT='(''perpsouth2 = '', 3e13.5)') PERPSOUTH2
         CALL VPROD(PERPSOUTH,PERPEAST,UPB2)
         WRITE (6,FMT='(''upb        = '', 3e13.5)') UPB
         WRITE (6,FMT='(''upb2       = '', 3e13.5)') UPB2
      END IF
C
C     .....Calculate l-shell parameter.....
      GCALT = RKM - 6378.16D0/DSQRT(1.D0+.0067397D0*ST*ST)
      CALL INVAR(TM1,GCLAT,GLON,GCALT,0.01D0,BB,RL)
      RL = MAX(RL,1.0D0)
C
C     .....Calculate dip latitude.....
      RLATM = ATAN(.5D0*TAN(DTR*AINC))/DTR
C
C     .....Calculate invariant latitude.....
      RLATI = ACOS(DSQRT(1.0D0/RL))/DTR
C
C     .....Convert radar propagation vector and observation point
C          position to earth centered cartesian coordinates.....
      CALL RPCART(SR,SLATGC,SLON,AZ,EL,RANGE,RFX,RFY,RFZ,PFX,PFY,PFZ)
C
C     .....Calculate earth-centered cartestian components of unit vector
C          perpendicular to radar propagation vector and geomagnetic
C          geomagnetic field at observation point.....
      CALL VPROD(RF,BF,EF)
      EFMAG = VMAG(EF)
      EFX = EFX/EFMAG
      EFY = EFY/EFMAG
      EFZ = EFZ/EFMAG
C
C     .....Calculate aspect angle.....
      CASPCT = SPROD(RF,BF)/(VMAG(RF)*VMAG(BF))
      CASPCT = MIN(MAX(CASPCT,-1.0D0),1.0D0)
      ASPCT = ACOS(CASPCT)/DTR
C
C     .....Calculate direction cosines of radar beam with respect to
C          geocentric south, east, up.....
      CALL VCTCNV(RFX,RFY,RFZ,PFX,PFY,PFZ,RFR,RFT,RFP,RR,RT,RP,1)
      CST = RFT/RANGE
      CSP = RFP/RANGE
      CSR = RFR/RANGE
C
C     .....Compute geodetic direction cosines. the direction cosines
C          are with respect to south, east, up rather than x (north),
C          y (east), z (down) as used in gdv.....
      CALL GDV(GDLAT,GCLAT,CSR,CST,CSP,CX,CY,CZ)
      CTAB(1) = -CX
      CTAB(2) = CY
      CTAB(3) = -CZ
C
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''rl,rlatm,rlati,aspct = '', 4f9.2)') RL,RLATM,
     *     RLATI,ASPCT
         WRITE (6,FMT='(''rfx,rfy,rfz = '', 3f10.2)') RFX,RFY,RFZ
         WRITE (6,FMT='(''cst,csp,csr = '', 3f9.5)') CST,CSP,CSR
         WRITE (6,FMT='(''ctab = '', 3f9.5)') CTAB
      END IF
C
C        .....Calculate observation point apex coordinates.....
      ISTOP = 0
      NPR = 0
      INIT = 0
      D = 1.0D-6
      CALL LINTRA(TM1,GCLAT,GLON,RKM,GDALT,0.0D0,PLAT,PLON,PRKM,ARC,
     *            ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
      Q0(1) = DTR*(90.0D0-ALAT)
      Q0(2) = DTR*ALON
C
C        .....Calculate arc length along field line from surface to
C             observation point.....
      ISTOP = -1
      CALL LINTRA(TM1,GCLAT,GLON,RKM,GDALT,0.0D0,PLAT,PLON,PRKM,ARC,
     *            ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
      Q0(3) = ARC/6370.0D0
C
C     .....Get conjugate point using lintra.....
      HI = DBLE(GDALT)
C
C     .....Tell lintra to find mag conj, not apex.....
      ISTOP = 1
      CALL LINTRA(TM,GCLAT,GLON,RKM,HI,GDALT,CPLAT,CPLON,CPRKM,CARC,
     *            CARAD,CALAT,CALON,ISTOP,NPR,INIT,IER)
C     .....Get geodetic latitude of conjugate point.....
      CALL CONVRT(2,CGDLAT,CGDALT,CPLAT,CPRKM)
      IF (CPLON.LT.0.0D0) THEN
         CPLON = CPLON + 360.0D0
      END IF
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
      RCOR(11) = RLATM
      RCOR(12) = RLATI
      RCOR(13) = RL
      RCOR(14) = ALAT
      RCOR(15) = ALON
c      RCOR(16) = GF(1)
c      RCOR(17) = GF(2)
c      RCOR(18) = GF(3)
c      RCOR(19) = GF(4)
      RCOR(16) = 0.0
      RCOR(17) = 0.0
      RCOR(18) = 0.0
      RCOR(19) = 0.0
      RCOR(20) = CTAB(1)
      RCOR(21) = CTAB(2)
      RCOR(22) = CTAB(3)
      RCOR(23) = 0.0
      RCOR(24) = 0.0
      RCOR(25) = 0.0
      RCOR(26) = 0.0
      RCOR(27) = 0.0
      RCOR(28) = 0.0
      RCOR(29) = AINC
      RCOR(30) = DEC
      RCOR(31) = GCLAT
      RCOR(32) = ASPCT
      RCOR(33) = CPLAT
      RCOR(34) = CGDLAT
      RCOR(35) = CPLON
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

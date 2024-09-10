C     $Id: coord.f 7593 2023-08-24 16:32:40Z brideout $
C
      SUBROUTINE COORD(SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,GDLAT,GLON,
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
C        The grad* parameters are gradients of the geomagnetic field B.
C        They measure the separation between field lines and hence the
C        variation of the electric field as a function of position.
C        this permits us to represent the perpendicular E field and
C        hence the E X B drift along a field line by a single
C        parameter. Note that only the relative value of these
C        parameters is significant.
C        along the field line.
C        RCOR(36) - gradapex1(1) - apex latitude component of gradient
C                   of B
C        RCOR(37) - gradapex2(2) - apex lonfitude component gradient
C                   of B
C        RCOR(38) - gradapex3(3) - up b component of gradient of B
C        gradmag = gradient of B converted to standard magnetic south,
C                  east, up
C        RCOR(39) - gradmag1(1) - gradapex1 magnetic south component
C        RCOR(40) - gradmag1(2) - gradapex1 magnetic east component
C        RCOR(41) - gradmag1(3) - gradapex1 up B component
C        RCOR(42) - gradmag2(1) - gradapex2 magnetic south component
C        RCOR(43) - gradmag2(2) - gradapex2 magnetic east component
C        RCOR(44) - gradmag3(3) - gradapex2 up B component
C        RCOR(45) - gradmag3(1) - gradapex3 magnetic south component
C        RCOR(46) - gradmag3(2) - gradapex3 magnetic east component
C        RCOR(47) - gradmag3(3) - gradapex3 up B component
C        The following three parameters are the direction cosines of
C        the radar beam wrt standard magnetic south, magnetic east
C        and up B directions. These are useful for expressing final
C        results but not for calculating the variation of E along a
C        field line because they do not define a coordinate system
C        (in which the gradient of a potential can be calculated) but
C        only a set of orthogonal directions.
C        RCOR(48) - ctab(7) - direction cosine of beam wrt vector
C                             perpendicular to B in magnetic meridian
C        RCOR(49) - ctab(8) - direction cosine of beam wrt vector
C                             perpendicular to ctab(7) and the magnetic
C                             meridian
C        RCOR(50) - ctab(9) - direction cosine of beam wrt up B
C        direction cosines of measured component of electric field wrt
C        apex contravariant unit vectors
C        RCOR(51) - efcoscn1 - wrt apex latitude
C        RCOR(52) - efcoscn1 - wrt apex longitude
C        RCOR(53) - efcoscn1 - wrt up B
C        RCOR(54) = ARC (?)
C        RCOR(55-64) - undefined
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
     *                 CARC,CASPCT,CGDALT,CGDLAT,COSCN1,COSCN2,COSCN3,
     *                 COSCO1,COSCO2,COSCO3,COST1,COST2,COST3,CP,CPLAT,
     *                 CPLON,CPRKM,CSP,CSR,CST,CT,CTABM,CX,CY,CZ,D,DEC,
     *                 DECGC,DET,DGCLAT,DGDALT,DGDLAT,DGLON,DRKM,DTR,
     *                 EAARC,EALAT,EALON,EASTM,EASTP,EASTR,EASTT,EASTX,
     *                 EASTY,EASTZ,ECN1M,ECN2M,ECN3M,ECO1M,ECO2M,ECO3M,
     *                 EFCOSCN1,EFCOSCN2,EFCOSCN3,EFMAG,EFX,EFY,EFZ,
     *                 GCALT,GCLAT,GDLATS,GRADAPEXM,GRADMAGM,GRADXYZM,
     *                 HB,HI,P,PERPEASTP,PERPEASTR,PERPEASTT,PERPSOUTHP,
     *                 PERPSOUTHR,PERPSOUTHT,PFX,PFY,PFZ,PHI,PLAT,PLON,
     *                 PRKM,PX,PY,PZ,Q1,Q2,Q3,R,RFP,RFR,RFT,RFX,RFY,RFZ,
     *                 RKM,RL,RLATI,RLATM,RMAG,RP,RR,RT,SOUTHM,SOUTHP,
     *                 SOUTHR,SOUTHT,SOUTHX,SOUTHY,SOUTHZ,SP,ST,T,THETA,
     *                 TM1,TP,TR,TT,UPBP,UPBR,UPBT,UPP,UPR,UPT,UPX,UPY,
     *                 UPZ,VCN,VCO,X1,X2,X3,XB,XDUM,YB,ZB
      INTEGER I,IER,INIT,ISTOP,J,K,NPR
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION BF(3),BPERPEAST(3),BPERPSOUTH(3),CTAB(9),
     *                 DQDX(3,3),DQDX1(3,3),DXDQ(3,3),DXDQSP(3,3),
     *                 EAST(3),EAST2(3),ECN1(3),ECN1X(3),ECN2(3),
     *                 ECN2X(3),ECN3(3),ECN3X(3),ECNU1(3),ECNU2(3),
     *                 ECNU3(3),ECO1(3),ECO1X(3),ECO2(3),ECO2X(3),
     *                 ECO3(3),ECO3X(3),EF(3),GCN(3,3),GCNSPHERE(3,3),
     *                 GCO(3,3),GCO1(3,3),GF(4),GRADAPEX(3),
     *                 GRADAPEXSP1(3),GRADAPEXSP2(3),GRADAPEXSP3(3),
     *                 GRADMAG(3),GRADXYZ(3),PERPEAST(3),PERPEAST2(3),
     *                 PERPSOUTH(3),PERPSOUTH2(3),PF(3),Q(3),Q0(3),
     *                 RF(3),SOUTH(3),SOUTH2(3),SV(3),TV(3),UP(3),
     *                 UP2(3),UPB(3),UPB2(3),X(3),X0(3),ZEROM(3)
      INTEGER LW(3),MW(3)
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
      Q1 = 0.0D0
      Q2 = 0.0D0
      THETA = 0.0D0
      R = 0.0D0
      PHI= 0.0D0
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
      IF (QSPHERICAL .NE. 0) THEN
         Q0(1) = DTR*(90.0D0-GCLAT)
         Q0(2) = DTR*GLON
         Q0(3) = RKM/6370.0D0
         THETA = Q0(1)
         PHI = Q0(2)
         R = Q0(3)
      END IF
C
C        .....Calculate apex-cartesian direction cosines.....
      X0(1) = PX/6370.0D0
      X0(2) = PY/6370.0D0
      X0(3) = PZ/6370.0D0
      X(1) = X0(1)
      X(2) = X0(2)
      X(3) = X0(3)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(3f7.1,2x,2f10.5,f9.3,2x,3f10.6,2x,3f10.6)') AZ,
     *     EL,RANGE,ALAT,ALON,ARC,Q0(1),Q0(2),Q0(3),X(1),X(2),X(3)
      END IF
      DO 20 J = 1,3
         X(J) = X(J) + D
         X1 = X(1)
         X2 = X(2)
         X3 = X(3)
         CALL CSCONV(6370.0D0*X1,6370.0D0*X2,6370.0D0*X3,DRKM,DGCLAT,
     *               DGLON,1)
         DGCLAT = 90.0D0 - DGCLAT
         CALL CONVRT(2,DGDLAT,DGDALT,DGCLAT,DRKM)
         ISTOP = 0
         CALL LINTRA(TM1,DGCLAT,DGLON,DRKM,DGDALT,0.0D0,PLAT,PLON,PRKM,
     *               ARC,ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
         ISTOP = -1
         CALL LINTRA(TM1,DGCLAT,DGLON,DRKM,GDALT,0.0D0,PLAT,PLON,PRKM,
     *               ARC,ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
         Q(1) = DTR*(90.0D0-ALAT)
         Q(2) = DTR*ALON
         Q(3) = ARC/6370.0D0
C
         IF (QSPHERICAL .NE. 0) THEN
            Q(1) = DTR*(90.0D0-DGCLAT)
            Q(2) = DTR*DGLON
            Q(3) = DRKM/6370.0D0
         END IF
         IF (QDIAG .NE. 0)  THEN
            WRITE (6,FMT='(3f7.1,2x,2f10.5,f9.3,2x,3f10.6,2x,3f10.6)')
     *        AZ,EL,RANGE,ALAT,ALON,ARC,Q(1),Q(2),Q(3),X(1),X(2),X(3)
         END IF
         X(J) = X0(J)
         DO 10 I = 1,3
            DQDX(I,J) = (Q(I)-Q0(I))/D
   10    CONTINUE
   20 CONTINUE
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''dqdx (finite differences) = '')')
         DO 30 I = 1,3
            WRITE (6,FMT='(3E13.5)') (DQDX(I,J),J=1,3)
   30    CONTINUE
      END IF
C
      DO 50 I = 1,3
         DO 40 J = 1,3
            DXDQ(I,J) = DQDX(I,J)
   40    CONTINUE
   50 CONTINUE
      CALL MINV(DXDQ,3,DET,LW,MW)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''dxdq (minv) = '')')
         DO 60 I = 1,3
            WRITE (6,FMT='(3E13.5)') (DXDQ(I,J),J=1,3)
   60    CONTINUE
         WRITE (6,FMT='(''det = '', E13.5)') DET
      END IF
C
      IF (QSPHERICAL .NE. 0) THEN
C           x = r*sin(theta)*cos(phi)
C           y = r*sin(theta)*sin(phi)
C           z = r*cos(theta)
         DXDQSP(1,1) = R*COS(THETA)*COS(PHI)
         DXDQSP(2,1) = R*COS(THETA)*SIN(PHI)
         DXDQSP(3,1) = -R*SIN(THETA)
         DXDQSP(1,2) = -R*SIN(THETA)*SIN(PHI)
         DXDQSP(2,2) = R*SIN(THETA)*COS(PHI)
         DXDQSP(3,2) = 0.0D0
         DXDQSP(1,3) = SIN(THETA)*COS(PHI)
         DXDQSP(2,3) = SIN(THETA)*SIN(PHI)
         DXDQSP(3,3) = COS(THETA)
         IF (QDIAG .NE. 0)  THEN
            WRITE (6,FMT='(''dxdq (spherical coordinates) = '')')
            DO 70 I = 1,3
               WRITE (6,FMT='(3E13.5)') (DXDQSP(I,J),J=1,3)
   70       CONTINUE
            WRITE (6,FMT='(''dxdqsp should be almost equal to dxdq'')')
            K = 0
            DO 90 I = 1,3
               DO 80 J = 1,3
                  IF (ABS(DXDQ(I,J)-DXDQSP(I,J)).GT.1.0D-05) K = K + 1
   80          CONTINUE
   90       CONTINUE
            IF (K.EQ.0) THEN
               WRITE (6,FMT='(''Test result: OK'')')
            ELSE
               WRITE (6,FMT='(''Test result: FAILED'')')
            END IF
            WRITE (6,FMT='('' '')')
         END IF
      END IF
C
      DO 110 I = 1,3
         DO 100 J = 1,3
            DQDX1(I,J) = DXDQ(I,J)
  100    CONTINUE
  110 CONTINUE
      CALL MINV(DQDX1,3,DET,LW,MW)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''dqdx1 (minv) = '')')
         DO 120 I = 1,3
            WRITE (6,FMT='(3E13.5)') (DQDX1(I,J),J=1,3)
  120    CONTINUE
         WRITE (6,FMT='(''det = '', E13.5)') DET
      END IF
C
C        .....Construct base vectors (= rows dqdx, dxdq).....
      DO 130 I = 1,3
         ECN1(I) = DQDX(1,I)
         ECN2(I) = DQDX(2,I)
         ECN3(I) = DQDX(3,I)
  130 CONTINUE
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''ecn1 = '', 3E13.5)') (ECN1(J),J=1,3)
         WRITE (6,FMT='(''ecn2 = '', 3E13.5)') (ECN2(J),J=1,3)
         WRITE (6,FMT='(''ecn3 = '', 3E13.5)') (ECN3(J),J=1,3)
      END IF
C
      DO 140 I = 1,3
         ECO1(I) = DXDQ(I,1)
         ECO2(I) = DXDQ(I,2)
         ECO3(I) = DXDQ(I,3)
  140 CONTINUE
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''eco1 = '', 3E13.5)') (ECO1(J),J=1,3)
         WRITE (6,FMT='(''eco2 = '', 3E13.5)') (ECO2(J),J=1,3)
         WRITE (6,FMT='(''eco3 = '', 3E13.5)') (ECO3(J),J=1,3)
      END IF
C
C        .....Compute covariant base vectors from contravariant
C             base vectors.....
      VCN = TPROD(ECN1,ECN2,ECN3)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''vcn = '', E13.5)') VCN
         WRITE (6,FMT='('' '')')
         CALL VRECIP(ECN1,ECN2,ECN3,ECO1X,ECO2X,ECO3X)
         WRITE (6,FMT='(''eco1x = '', 3E13.5)') (ECO1X(J),J=1,3)
         WRITE (6,FMT='(''eco2x = '', 3E13.5)') (ECO2X(J),J=1,3)
         WRITE (6,FMT='(''eco3x = '', 3E13.5)') (ECO3X(J),J=1,3)
      END IF
C
      VCO = TPROD(ECO1,ECO2,ECO3)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''vco = '', E13.5)') VCO
         WRITE (6,FMT='('' '')')
      END IF
      CALL VRECIP(ECO1,ECO2,ECO3,ECN1X,ECN2X,ECN3X)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''ecn1x = '', 3E13.5)') (ECN1X(J),J=1,3)
         WRITE (6,FMT='(''ecn2x = '', 3E13.5)') (ECN2X(J),J=1,3)
         WRITE (6,FMT='(''ecn3x = '', 3E13.5)') (ECN3X(J),J=1,3)
C
         WRITE (6,FMT='('' '')')
         SP = SPROD(ECN1,ECO1)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN1,ECO2)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN1,ECO3)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN2,ECO1)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN2,ECO2)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN2,ECO3)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN3,ECO1)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN3,ECO2)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
         SP = SPROD(ECN3,ECO3)
         WRITE (6,FMT='(''sp = '', E13.5)') SP
      END IF
C
C        .....Calculate covariant apex metric tensor from
C             derivative matrix.....
      CALL GMET(DXDQ,GCO)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''gco [gmet(dxdq)] = '')')
         DO 150 I = 1,3
            WRITE (6,FMT='(3E13.5)') (GCO(I,J),J=1,3)
  150    CONTINUE
      END IF
C
C        .....Calculate contravariant apex metric tensor from
C             derivative matrix.....
      CALL MTRAN3(DQDX)
      CALL GMET(DQDX,GCN)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''gcn [gmet(dqdx)] = '')')
         DO 160 I = 1,3
            WRITE (6,FMT='(3E13.5)') (GCN(I,J),J=1,3)
  160    CONTINUE
      END IF
C
C        .....Calculate covariant apex metric tensor from
C             base vectors.....
      CALL GMETBV(ECO1,ECO2,ECO3,GCO)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''gco [gmetbv(eco)] = '')')
         DO 170 I = 1,3
            WRITE (6,FMT='(3E13.5)') (GCO(I,J),J=1,3)
  170    CONTINUE
      END IF
C
C        .....Calculate contravariant apex metric tensor from
C             base vectors.....
      CALL GMETBV(ECN1,ECN2,ECN3,GCN)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''gcn [gmetbv(ecn)] = '')')
         DO 180 I = 1,3
            WRITE (6,FMT='(3E13.5)') (GCN(I,J),J=1,3)
  180    CONTINUE
      END IF
C
C        .....Calculate apex contravariant metric tensor.....
      DO 200 I = 1,3
         DO 190 J = 1,3
            GCN(I,J) = GCO(I,J)
  190    CONTINUE
  200 CONTINUE
      CALL MINV(GCN,3,DET,LW,MW)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''gcn [minv(gco)] = '')')
         DO 210 I = 1,3
            WRITE (6,FMT='(3E13.5)') (GCN(I,J),J=1,3)
  210    CONTINUE
         WRITE (6,FMT='(''det = '', e13.5)') DET
      END IF
C
      DO 230 I = 1,3
         DO 220 J = 1,3
            GCO1(I,J) = GCN(I,J)
  220    CONTINUE
  230 CONTINUE
      CALL MINV(GCO1,3,DET,LW,MW)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''gco1 [minv(gcn)] = '')')
         DO 240 I = 1,3
            WRITE (6,FMT='(3E13.5)') (GCO1(I,J),J=1,3)
  240    CONTINUE
         WRITE (52,FMT='(f8.1,4e13.5)') GDALT,GCN(1,1),GCN(1,2),
     *     GCN(2,1),GCN(2,2)
         WRITE (6,FMT='(''det = '', e13.5)') DET
         WRITE (6,FMT='('' '')')
      END IF
C
C        .....Compute metric tensor for spherical coordinates.....
      IF (QSPHERICAL .NE. 0) THEN
         Q1 = Q0(3)
         Q2 = Q0(3)*SIN(Q0(1))
         Q3 = 1.0d0
         DO 260 I = 1,3
            DO 250 J = 1,3
               GCNSPHERE(I,J) = 0.0D0
  250       CONTINUE
  260    CONTINUE
         GCNSPHERE(1,1) = 1.0d0/Q1**2
         GCNSPHERE(2,2) = 1.0d0/Q2**2
         GCNSPHERE(3,3) = 1.0d0/Q3**2
         IF (QDIAG .NE. 0)  THEN
            WRITE (6,FMT='('' '')')
            WRITE (6,FMT='(''Q**2 = '', 3e13.5)') Q1**2,Q2**2,Q3**2
            WRITE (6,FMT='(''1/Q**2 = '', 3e13.5)') 1.0d0/Q1**2,
     *        1.0d0/Q2**2,1.0d0/Q3**2
            WRITE (6,FMT='(''gcn [spherical coordinates] = '')')
            DO 270 I = 1,3
               WRITE (6,FMT='(3E13.5)') (GCNSPHERE(I,J),J=1,3)
  270       CONTINUE
            WRITE (6,FMT='(''Gcnsphere should be almost equal to gcn'')'
     *        )
            K = 0
            DO 290 I = 1,3
               DO 280 J = 1,3
                  IF (ABS(GCN(I,J)-GCNSPHERE(I,J)).GT.1.0D-05) K = K + 1
  280          CONTINUE
  290       CONTINUE
            IF (K.EQ.0) THEN
               WRITE (6,FMT='(''Test result: OK'')')
            ELSE
               WRITE (6,FMT='(''Test result: FAILED'')')
            END IF
            WRITE (6,FMT='('' '')')
         END IF
      END IF
C
C        .....Compute direction cosines.....
      ECO1M = VMAG(ECO1)
      ECO2M = VMAG(ECO2)
      ECO3M = VMAG(ECO3)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''eco1m,eco2m,eco3m = '', 3e13.5)') ECO1M,ECO2M,
     *     ECO3M
         ECO1M = VMAG(DXDQ(1,1))
         ECO2M = VMAG(DXDQ(1,2))
         ECO3M = VMAG(DXDQ(1,3))
         WRITE (6,FMT='(''eco1m,eco2m,eco3m = '', 3e13.5)') ECO1M,ECO2M,
     *     ECO3M
      END IF
      COSCO1 = SPROD(ECO1,RF)
      COSCO2 = SPROD(ECO2,RF)
      COSCO3 = SPROD(ECO3,RF)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''cosco1,cosco2,cosco3 = '', 3e13.5)') COSCO1,
     *     COSCO2,COSCO3
         COSCO1 = SPROD(DXDQ(1,1),RF)
         COSCO2 = SPROD(DXDQ(1,2),RF)
         COSCO3 = SPROD(DXDQ(1,3),RF)
         WRITE (6,FMT='(''cosco1,cosco2,cosco3 = '', 3e13.5)') COSCO1,
     *     COSCO2,COSCO3
      END IF
C
      ECN1M = VMAG(ECN1)
      ECN2M = VMAG(ECN2)
      ECN3M = VMAG(ECN3)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''ecn1m,ecn2m,ecn3m = '', 3e13.5)') ECN1M,ECN2M,
     *     ECN3M
         ECN1M = VMAG(DQDX(1,1))
         ECN2M = VMAG(DQDX(1,2))
         ECN3M = VMAG(DQDX(1,3))
         WRITE (6,FMT='(''ecn1m,ecn2m,ecn3m = '', 3e13.5)') ECN1M,ECN2M,
     *     ECN3M
      END IF
      CALL UVECT(ECN1,ECNU1)
      CALL UVECT(ECN2,ECNU2)
      CALL UVECT(ECN3,ECNU3)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''sprod(ecnu1,ecnu1) = '', e13.5)') SPROD(ECNU1,
     *     ECNU1)
         WRITE (6,FMT='(''sprod(ecnu1,ecnu2) = '', e13.5)') SPROD(ECNU1,
     *     ECNU2)
         WRITE (6,FMT='(''sprod(ecnu1,ecnu3) = '', e13.5)') SPROD(ECNU1,
     *     ECNU3)
         WRITE (6,FMT='(''sprod(ecnu2,ecnu1) = '', e13.5)') SPROD(ECNU2,
     *     ECNU1)
         WRITE (6,FMT='(''sprod(ecnu2,ecnu2) = '', e13.5)') SPROD(ECNU2,
     *     ECNU2)
         WRITE (6,FMT='(''sprod(ecnu2,ecnu3) = '', e13.5)') SPROD(ECNU2,
     *     ECNU3)
         WRITE (6,FMT='(''sprod(ecnu3,ecnu1) = '', e13.5)') SPROD(ECNU3,
     *     ECNU1)
         WRITE (6,FMT='(''sprod(ecnu3,ecnu2) = '', e13.5)') SPROD(ECNU3,
     *     ECNU2)
         WRITE (6,FMT='(''sprod(ecnu3,ecnu3) = '', e13.5)') SPROD(ECNU3,
     *     ECNU3)
         WRITE (6,FMT='(''sprod1 = '', e13.5)') SPROD(ECN1,BF)
         WRITE (6,FMT='(''sprod2 = '', e13.5)') SPROD(ECN2,BF)
         WRITE (6,FMT='(''sprod3 = '', e13.5)') SPROD(ECN3,BF)
         WRITE (6,FMT='(''sprod1 = '', e13.5)') SPROD(ECO1,BF)
         WRITE (6,FMT='(''sprod2 = '', e13.5)') SPROD(ECO2,BF)
         WRITE (6,FMT='(''sprod3 = '', e13.5)') SPROD(ECO3,BF)
      END IF
      COSCN1 = SPROD(ECN1,RF)
      COSCN2 = SPROD(ECN2,RF)
      COSCN3 = SPROD(ECN3,RF)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''coscn1,coscn2,coscn3 = '', 3e13.5)') COSCN1,
     *     COSCN2,COSCN3
         COSCN1 = SPROD(DQDX(1,1),RF)
         COSCN2 = SPROD(DQDX(1,2),RF)
         COSCN3 = SPROD(DQDX(1,3),RF)
         WRITE (6,FMT='(''coscn1,coscn2,coscn3 = '', 3e13.5)') COSCN1,
     *     COSCN2,COSCN3
      END IF
C
      RMAG = VMAG(RF)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''rmag = '', e13.5)') RMAG
      END IF
C
      CTAB(4) = COSCN1/(ECN1M*RMAG)
      CTAB(5) = COSCN2/(ECN2M*RMAG)
      CTAB(6) = COSCN3/(ECN3M*RMAG)
      CTAB(4) = SPROD(ECN1,RF)/(VMAG(ECN1)*VMAG(RF))
      CTAB(5) = SPROD(ECN2,RF)/(VMAG(ECN2)*VMAG(RF))
      CTAB(6) = SPROD(BF,RF)/(VMAG(BF)*VMAG(RF))
      CTABM = SQRT(CTAB(4)**2+CTAB(5)**2+CTAB(6)**2)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='('' '')')
         WRITE (6,FMT='(''ctab(4),ctab(5),ctab(6),ctabm = '', 4e13.5)')
     *     CTAB(4),CTAB(5),CTAB(6),CTABM
         WRITE (6,FMT='('' '')')
      END IF
C
C     Compute direction cosines with respect to contravariant unit
C     vectors of measured component of electric field.
      EFCOSCN1 = SPROD(ECN1,EF)/(VMAG(ECN1)*VMAG(EF))
      EFCOSCN2 = SPROD(ECN2,EF)/(VMAG(ECN2)*VMAG(EF))
      EFCOSCN3 = SPROD(BF,EF)/(VMAG(BF)*VMAG(EF))
C
      GF(1) = GCN(1,1)*ECO1M
      GF(2) = GCN(2,1)*ECO1M
      GF(3) = GCN(1,2)*ECO2M
      GF(4) = GCN(2,2)*ECO2M
C
C     .....Here we assert that the field line is an equipotential.....
      EALAT = 1.0D0
      EALON = 1.0D0
      EAARC = 1.0D0
C
      IF (QSPHERICAL .NE. 0) THEN
         GRADAPEXSP1(1) = 1/Q1
         GRADAPEXSP1(2) = 0.0D0
         GRADAPEXSP1(3) = 0.0D0
         GRADAPEXSP2(1) = 0.0D0
         GRADAPEXSP2(2) = 1/Q2
         GRADAPEXSP2(3) = 0.0D0
         GRADAPEXSP3(1) = 0.0D0
         GRADAPEXSP3(2) = 0.0D0
         GRADAPEXSP3(3) = 0.0D0
         GRADAPEXSP3(3) = 1/Q3
         IF (QDIAG .NE. 0)  THEN
            WRITE (6,FMT='(''gradapexsp = '')')
            WRITE (6,FMT='(4f9.5,2x,4f9.5)') GRADAPEXSP1,
     *        VMAG(GRADAPEXSP1)
            WRITE (6,FMT='(4f9.5,2x,4f9.5)') GRADAPEXSP2,
     *        VMAG(GRADAPEXSP2)
            WRITE (6,FMT='(4f9.5,2x,4f9.5/)') GRADAPEXSP3,
     *        VMAG(GRADAPEXSP3)
         END IF
      END IF
C
      GRADAPEX(1) = 1.0D0
      GRADAPEX(2) = 1.0D0
      GRADAPEX(3) = 0.0D0
      GRADXYZ(1) = ECN1(1)*GRADAPEX(1) + ECN2(1)*GRADAPEX(2) +
     *             ECN3(1)*GRADAPEX(3)
      GRADXYZ(2) = ECN1(2)*GRADAPEX(1) + ECN2(2)*GRADAPEX(2) +
     *             ECN3(2)*GRADAPEX(3)
      GRADXYZ(3) = ECN1(3)*GRADAPEX(1) + ECN2(3)*GRADAPEX(2) +
     *             ECN3(3)*GRADAPEX(3)
      GRADMAG(1) = SPROD(PERPSOUTH,GRADXYZ)
      GRADMAG(2) = SPROD(PERPEAST,GRADXYZ)
      GRADMAG(3) = SPROD(UPB,GRADXYZ)
      GRADAPEXM = VMAG(GRADAPEX)
      GRADXYZM = VMAG(GRADXYZ)
      GRADMAGM = VMAG(GRADMAG)
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''gclat,glon,sr,gdalt = '', 4f10.2/)') GCLAT,
     *     GLON,SR,GDALT
         WRITE (6,FMT='(''eco1,ecn1 = '', 3f9.5,2x,3f9.5,2x,f9.5)')
     *     ECO1,ECN1,SPROD(ECO1,ECN1)
         WRITE (6,FMT='(''eco2,ecn2 = '', 3f9.5,2x,3f9.5,2x,f9.5)')
     *     ECO2,ECN2,SPROD(ECO2,ECN2)
         WRITE (6,FMT='(''eco3,ecn3 = '', 3f9.5,2x,3f9.5,2x,f9.5/)')
     *     ECO3,ECN3,SPROD(ECO3,ECN3)
         WRITE (6,FMT='(''gradapex,gradxyz = '')')
         WRITE (6,FMT='(4f9.5,2x,4f9.5)') GRADAPEX,VMAG(GRADAPEX),
     *     GRADXYZ,VMAG(GRADXYZ)
      END IF
      IF ((QSPHERICAL .NE. 0) .AND. (QDIAG .NE. 0)) THEN
         WRITE (6,FMT='(''theta,phi = '', 2f9.5)') THETA/DTR,PHI/DTR
         WRITE (6,FMT='(''gradxyzsp = '')')
         WRITE (6,FMT='(3f9.5)') COS(THETA)*COS(PHI),
     *     COS(THETA)*SIN(PHI),-SIN(THETA)
         WRITE (6,FMT='(3f9.5)') - SIN(PHI),COS(PHI),0.0D0
         WRITE (6,FMT='(3f9.5/)') SIN(THETA)*COS(PHI),
     *     SIN(THETA)*SIN(PHI),COS(THETA)
      END IF
C
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''sprod11 = '', f10.5)') SPROD(GRADXYZ,GRADXYZ)
         WRITE (6,FMT='(3f10.5,2x,4f10.5,2x,4f10.5)') GRADAPEX,GRADXYZ,
     *     VMAG(GRADXYZ),GRADMAG,VMAG(GRADMAG)
         WRITE (6,FMT='(''bf = '', 4f10.5)') BF,VMAG(BF)
         WRITE (6,FMT='(''sprod(gradxyz,bf) = '', f10.5)')
     *     SPROD(GRADXYZ,BF)
      END IF
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''decgc,ainc = '', 2f10.5)') DECGC,AINC
         WRITE (6,FMT='(''perpsouth = '', 4f10.5)') PERPSOUTH,
     *     VMAG(PERPSOUTH)
         WRITE (6,FMT='(''perpeast = '', 4f10.5)') PERPEAST,
     *     VMAG(PERPEAST)
         WRITE (6,FMT='(''upb = '', 4f10.5)') UPB,VMAG(UPB)
         WRITE (6,FMT='(''gradmag1 = '', 3f10.5)') GRADMAG
         WRITE (6,FMT=
     *'(''Vectors 1, 2 and 3 must have the same length in '',
     *    ''all three coordinate systems'')')
         WRITE (6,FMT='(3f9.5,2x,3f9.5,2x,3f9.5,2x,3f9.5)')
     *     SPROD(GRADAPEX,GRADAPEX),SPROD(GRADXYZ,GRADXYZ)
      END IF
      CTAB(7) = SPROD(PERPSOUTH,RF)
      CTAB(8) = SPROD(PERPEAST,RF)
      CTAB(9) = SPROD(UPB,RF)
      CTABM = VMAG(CTAB(7))
      CTAB(7) = CTAB(7)/CTABM
      CTAB(8) = CTAB(8)/CTABM
      CTAB(9) = CTAB(9)/CTABM
C
      IF (QDIAG .NE. 0)  THEN
         WRITE (6,FMT='(''CTAB(7),CTAB(8),CTAB(9) = '')')
         WRITE (6,FMT='(4E13.5)') CTAB(7),CTAB(8),CTAB(9),CTABM
      END IF
C
C        .....Spherical coordinate test case.....
      IF (QSPHERICAL .NE. 0) THEN
         GF(1) = GCN(1,1)*ECO2M
         GF(2) = GCN(2,1)*ECO2M
         GF(3) = GCN(1,2)*ECO3M
         GF(4) = GCN(2,2)*ECO3M
         IF (QDIAG .NE. 0)  THEN
            WRITE (6,FMT='(''gf = '')')
            WRITE (6,FMT='(2e13.5)') GF(1),GF(2),GF(3),GF(4)
            WRITE (6,FMT='(''1/Q = '', 3e13.5)') 1.0d0/Q1,1.0d0/Q2,
     *        1.0d0/Q3
         END IF
      END IF
C
C        .....Calculate direction cosines of a vector perpendicular to
C            the line-of-sight with respect to the apex coordinates.....
      TT = -DSIN(DTR*(180.D0-AZ))
      TP = DCOS(DTR*(180.D0-AZ))
      TR = 0.D0
      CALL VCTCNV(TV(1),TV(2),TV(3),SV(1),SV(2),SV(3),TR,TT,TP,SR,
     *            90.D0-SLATGC,SLON,2)
      COST1 = SPROD(DQDX(1,1),TV)/ECN1M
      COST2 = SPROD(DQDX(1,2),TV)/ECN2M
      COST3 = SPROD(DQDX(1,3),TV)/ECN3M
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
      RCOR(16) = GF(1)
      RCOR(17) = GF(2)
      RCOR(18) = GF(3)
      RCOR(19) = GF(4)
      RCOR(20) = CTAB(1)
      RCOR(21) = CTAB(2)
      RCOR(22) = CTAB(3)
      RCOR(23) = CTAB(4)
      RCOR(24) = CTAB(5)
      RCOR(25) = CTAB(6)
      RCOR(26) = COST1
      RCOR(27) = COST2
      RCOR(28) = COST3
      RCOR(29) = AINC
      RCOR(30) = DEC
      RCOR(31) = GCLAT
      RCOR(32) = ASPCT
      RCOR(33) = CPLAT
      RCOR(34) = CGDLAT
      RCOR(35) = CPLON
      RCOR(36) = GRADAPEX(1)
      RCOR(37) = GRADAPEX(2)
      RCOR(38) = GRADAPEX(3)
      RCOR(39) = GRADXYZ(1)
      RCOR(40) = GRADXYZ(2)
      RCOR(41) = GRADXYZ(3)
      RCOR(42) = GRADMAG(1)
      RCOR(43) = GRADMAG(2)
      RCOR(44) = GRADMAG(3)
      RCOR(45) = GRADMAG(1)*B
      RCOR(46) = GRADMAG(2)*B
      RCOR(47) = GRADMAG(3)*B
      RCOR(45) = GRADMAG(1)
      RCOR(46) = GRADMAG(2)
      RCOR(47) = GRADMAG(3)
      RCOR(48) = CTAB(7)
      RCOR(49) = CTAB(8)
      RCOR(50) = CTAB(9)
      RCOR(51) = EFCOSCN1
      RCOR(52) = EFCOSCN2
      RCOR(53) = EFCOSCN3
      RCOR(54) = ARC
C
      RETURN
      END

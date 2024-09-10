c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
C     Downloaded from ftp://hanna.ccmc.gsfc.nasa.gov/pub/modelweb/magnetospheric/tsyganenko
C     by B. Rideout on Mar. 29, 2013.  
C     The following changes were made from imported version:
C
C     1. ran script modifyFortran.py to remove
C        to avoid conflicts with geo-cgm
C     2. remake_file script (nag_apt + tcl) run
C     3. All print statements commented out
C     4. Some _08 methods renamed to drop _08 due to compiling issues with double underscores
C     5. added common block br_err to signal errors back to trace subroutine
c
c     ##########################################################################
c     #                                                                        #
c     #                             GEOPACK-2008                               #
c     #                     (MAIN SET OF FORTRAN CODES)                        #
c     #              (IGRF-11 coefficients added on Nov 30, 2010)              #
c     ##########################################################################
C
c
cThis collection of subroutines is a result of several upgrades of the original package
c     written by N. A. Tsyganenko in 1978-1979.
c
c     PREFATORY NOTE TO THE VERSION OF FEBRUARY 8, 2008:
c
cTo avoid inappropriate use of obsolete subroutines from earlier versions, a suffix 08 was
c     added to the name of each subroutine in this release.
c
cA possibility has been added in this version to calculate vector components in the
c"Geocentric Solar Wind" (GSW) coordinate system, which, to our knowledge, was first
cintroduced by Hones et al., Planet. Space Sci., v.34, p.889, 1986 (aka GSWM, see Appendix,
cTsyganenko et al., JGRA, v.103(A4), p.6827, 1998). The GSW system is analogous to the
cstandard GSM, except that its X-axis is antiparallel to the currently observed solar wind
cflow vector, rather than aligned with the Earth-Sun line. The orientation of axes in the
cGSW system can be uniquely defined by specifying three components (VGSEX,VGSEY,VGSEZ) of
cthe solar wind velocity, and in the case of a strictly radial anti-sunward flow (VGSEY=
cVGSEZ=0) the GSW system becomes identical to the standard GSM, which fact was used here
cto minimize the number of subroutines in the package. To that end, instead of the special
ccase of the GSM coordinates, this version uses a more general GSW system, and three more
cinput parameters are added in the subroutine TS_RECALC_08, the observed values (VGSEX,VGSEY,
cVGSEZ) of the solar wind velocity. Invoking TS_RECALC_08 with VGSEY=VGSEZ=0 restores the
cstandard (sunward) orientation of the X axis, which allows one to easily convert vectors
cbetween GSW and GSM, as well as to/from other standard and commonly used systems. For more
c     details, see the documentation file GEOPACK-2008.DOC.
c
cAnother modification allows users to have more control over the procedure of field line
cmapping using the subroutine TRACE. To that end, three new input parameters were added
cin that subroutine, allowing one to set (i) an upper limit, DSMAX, on the automatically
cadjusted step size, (ii) a permissible step error, ERR, and (iii) maximal length, LMAX,
cof arrays where field line point coordinates are stored. Minor changes were also made in
cthe tracing subroutine, to make it more compact and easier for understanding, and to
cprevent the algorithm from making uncontrollable large number of multiple loops in some
c     cases with plasmoid-like field structures.
c
COne more subroutine, named GEODGEO_08, was added to the package, allowing one to convert
cgeodetic coordinates of a point in space (altitude above the Earth's WGS84 ellipsoid and
cgeodetic latitude) to geocentric radial distance and colatitude, and vice versa.
c
CFor a complete list of modifications made earlier in previous versions, see the
c     documentation file GEOPACK-2008.DOC.
c
c----------------------------------------------------------------------------------
c
      SUBROUTINE IGRF_GSW_08(XGSW,YGSW,ZGSW,HXGSW,HYGSW,HZGSW)
c
CCALCULATES COMPONENTS OF THE MAIN (INTERNAL) TS_GEOMAGNETIC FIELD IN THE GEOCENTRIC SOLAR-WIND
C(GSW) COORDINATE SYSTEM, USING IAGA INTERNATIONAL TS_GEOMAGNETIC REFERENCE MODEL COEFFICIENTS
C(e.g., http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html, revised 22 March, 2005)
c
CTHE GSW SYSTEM IS ESSENTIALLY SIMILAR TO THE STANDARD GSM (THE TWO SYSTEMS BECOME IDENTICAL
CTO EACH OTHER IN THE CASE OF STRICTLY ANTI-TS_SUNWARD SOLAR WIND FLOW). FOR A DETAILED
C   DEFINITION, SEE INTRODUCTORY COMMENTS FOR THE SUBROUTINE GSWGSE .
C
CBEFORE THE FIRST CALL OF THIS SUBROUTINE, OR, IF THE DATE/TIME (IYEAR,IDAY,IHOUR,MIN,ISEC),
COR THE SOLAR WIND VELOCITY COMPONENTS (VGSEX,VGSEY,VGSEZ) HAVE CHANGED, THE MODEL COEFFICIENTS
cAND GEO-GSW ROTATION MATRIX ELEMENTS SHOULD BE UPDATED BY CALLING THE SUBROUTINE TS_RECALC_08.
C
C     -----INPUT PARAMETERS:
C
CXGSW,YGSW,ZGSW - CARTESIAN GEOCENTRIC SOLAR-WIND COORDINATES (IN UNITS RE=6371.2 KM)
C
C     -----OUTPUT PARAMETERS:
C
CHXGSW,HYGSW,HZGSW - CARTESIAN GEOCENTRIC SOLAR-WIND COMPONENTS OF THE MAIN TS_GEOMAGNETIC
C                           FIELD IN NANOTESLA
C
C     LAST MODIFICATION:  FEB 07, 2008.
C     THIS VERSION OF THE CODE ACCEPTS DATES FROM 1965 THROUGH 2015.
c
C     AUTHOR: N. A. TSYGANENKO
C
C


C     .. Scalar Arguments ..
      DOUBLE PRECISION HXGSW,HYGSW,HZGSW,XGSW,YGSW,ZGSW
      DOUBLE PRECISION XGEO,YGEO,ZGEO,R,THETA,PHI,COLAT,ELONG
      DOUBLE PRECISION BN,BE,BZ,B
C     .. common block added by B. Rideout to get date from RECALC_08
      COMMON /BR_FYEAR/FYEAR
      DOUBLE PRECISION FYEAR
C     ..
C     Rewritten by Bill Rideout.  Now calls call igrf13.f methods
C
C  convert to position to X,Y,Z
      CALL GEOGSW(XGEO,YGEO,ZGEO,XGSW,YGSW,ZGSW,-1)
C  convert to spherical position
      CALL TS_SPHCAR_08(R,THETA,PHI,XGEO,YGEO,ZGEO,-1)
C  convert to R, COLAT, ELONG
      R = R * 6378.1
      COLAT = THETA * 57.295779
      ELONG = PHI * 57.295779
C  call igrf
      CALL igrf13syn(0,FYEAR,2,R,COLAT,ELONG,BN,BE,BZ,B)
      BZ = -1.0 * BZ
C  convert to X,Y,Z coordinates
      CALL TS_BSPCAR_08(THETA,PHI,BZ,-1.0*BN,BE,XGEO,YGEO,ZGEO)
      CALL GEOGSW(XGEO,YGEO,ZGEO,HXGSW,HYGSW,HZGSW,1)
      RETURN
      END
C
c==========================================================================================
c
      SUBROUTINE IGRF_GEO_08(R,THETA,PHI,BR,BTHETA,BPHI)
c
CCALCULATES COMPONENTS OF THE MAIN (INTERNAL) TS_GEOMAGNETIC FIELD IN THE SPHERICAL GEOGRAPHIC
C(GEOCENTRIC) COORDINATE SYSTEM, USING IAGA INTERNATIONAL TS_GEOMAGNETIC REFERENCE MODEL
CCOEFFICIENTS  (e.g., http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html, revised: 22 March, 2005)
C
CBEFORE THE FIRST CALL OF THIS SUBROUTINE, OR IF THE DATE (IYEAR AND IDAY) WAS CHANGED,
CTHE MODEL COEFFICIENTS SHOULD BE UPDATED BY CALLING THE SUBROUTINE TS_RECALC_08
C
C     -----INPUT PARAMETERS:
C
C     R, THETA, PHI - SPHERICAL GEOGRAPHIC (GEOCENTRIC) COORDINATES:
CRADIAL DISTANCE R IN UNITS RE=6371.2 KM, COLATITUDE THETA AND LONGITUDE PHI IN RADIANS
C
C     -----OUTPUT PARAMETERS:
C
CBR, BTHETA, BPHI - SPHERICAL COMPONENTS OF THE MAIN TS_GEOMAGNETIC FIELD IN NANOTESLA
C      (POSITIVE BR OUTWARD, BTHETA SOUTHWARD, BPHI EASTWARD)
C
C     LAST MODIFICATION:  MAY 4, 2005.
C     THIS VERSION OF THE  CODE ACCEPTS DATES FROM 1965 THROUGH 2015.
c
C     AUTHOR: N. A. TSYGANENKO
C
C


C     .. Scalar Arguments ..
      DOUBLE PRECISION BPHI,BR,BTHETA,PHI,R,THETA
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AN,BBF,BBR,BBT,BI,C,CF,D,D2,DP,E,HH,P,P2,PM,PP,Q,
     *                 QQ,S,SF,W,X,XK,Y,Z
      INTEGER IRP3,K,M,MM,MN,N,NM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A(14),B(14)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC COS,SIN
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK2/G,H,REC
      DOUBLE PRECISION G(105),H(105),REC(105)
C     ..
      C = COS(THETA)
      S = SIN(THETA)
      CF = COS(PHI)
      SF = SIN(PHI)
C
      PP = 1.D0/R
      P = PP
C
CIN THIS NEW VERSION, THE OPTIMAL VALUE OF THE PARAMETER NM (MAXIMAL ORDER OF THE SPHERICAL
CHARMONIC EXPANSION) IS NOT USER-PRESCRIBED, BUT CALCULATED INSIDE THE SUBROUTINE, BASED
C      ON THE VALUE OF THE RADIAL DISTANCE R:
C
      IRP3 = R + 2
      NM = 3 + 30/IRP3
      IF (NM.GT.13) NM = 13

      K = NM + 1
      DO 10 N = 1,K
         P = P*PP
         A(N) = P
         B(N) = P*N
   10 CONTINUE

      P = 1.D0
      D = 0.D0
      BBR = 0.D0
      BBT = 0.D0
      BBF = 0.D0

      DO 60 M = 1,K
         IF (M.EQ.1) GO TO 20
         MM = M - 1
         W = X
         X = W*CF + Y*SF
         Y = Y*CF - W*SF
         GO TO 30
   20    X = 0.D0
         Y = 1.D0
   30    Q = P
         Z = D
         BI = 0.D0
         P2 = 0.D0
         D2 = 0.D0
         DO 50 N = M,K
            AN = A(N)
            MN = N*(N-1)/2 + M
            E = G(MN)
            HH = H(MN)
            W = E*Y + HH*X
            BBR = BBR + B(N)*W*Q
            BBT = BBT - AN*W*Z
            IF (M.EQ.1) GO TO 40
            QQ = Q
            IF (S.LT.1.D-5) QQ = Z
            BI = BI + AN*(E*X-HH*Y)*QQ
   40       XK = REC(MN)
            DP = C*Z - S*Q - XK*D2
            PM = C*Q - XK*P2
            D2 = Z
            P2 = Q
            Z = DP
            Q = PM
   50    CONTINUE
         D = S*D + C*P
         P = S*P
         IF (M.EQ.1) GO TO 60
         BI = BI*MM
         BBF = BBF + BI
   60 CONTINUE
C
      BR = BBR
      BTHETA = BBT
      IF (S.LT.1.D-5) GO TO 70
      BPHI = BBF/S
      RETURN
   70 IF (C.LT.0.D0) BBF = -BBF
      BPHI = BBF

      RETURN
      END
C
c==========================================================================================
c
      SUBROUTINE DIP_08(XGSW,YGSW,ZGSW,BXGSW,BYGSW,BZGSW)
C
CCALCULATES GSW (GEOCENTRIC SOLAR-WIND) COMPONENTS OF GEODIPOLE FIELD WITH THE DIPOLE MOMENT
CCORRESPONDING TO THE EPOCH, SPECIFIED BY CALLING SUBROUTINE TS_RECALC_08 (SHOULD BE
CINVOKED BEFORE THE FIRST USE OF THIS ONE, OR IF THE DATE/TIME, AND/OR THE OBSERVED
C     SOLAR WIND DIRECTION, HAVE CHANGED.
C
CTHE GSW COORDINATE SYSTEM IS ESSENTIALLY SIMILAR TO THE STANDARD GSM (THE TWO SYSTEMS BECOME
CIDENTICAL TO EACH OTHER IN THE CASE OF STRICTLY RADIAL ANTI-TS_SUNWARD SOLAR WIND FLOW). ITS
CDETAILED DEFINITION IS GIVEN IN INTRODUCTORY COMMENTS FOR THE SUBROUTINE GSWGSE .

C--INPUT PARAMETERS: XGSW,YGSW,ZGSW - GSW COORDINATES IN RE (1 RE = 6371.2 km)
C
C--OUTPUT PARAMETERS: BXGSW,BYGSW,BZGSW - FIELD COMPONENTS IN GSW SYSTEM, IN NANOTESLA.
C
C     LAST MODIFICATION: JAN 28, 2008.
C
C     AUTHOR: N. A. TSYGANENKO
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BXGSW,BYGSW,BZGSW,XGSW,YGSW,ZGSW
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DIPMOM,P,Q,T,U,V
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/AA,SPS,CPS,BB
      COMMON /GEOPACK2/G,H,REC
      DOUBLE PRECISION CPS,SPS
      DOUBLE PRECISION AA(10),BB(22),G(105),H(105),REC(105)
C     ..
      DIPMOM = SQRT(G(2)**2+G(3)**2+H(3)**2)
C
      P = XGSW**2
      U = ZGSW**2
      V = 3.D0*ZGSW*XGSW
      T = YGSW**2
      Q = DIPMOM/SQRT(P+T+U)**5
      BXGSW = Q*((T+U-2.D0*P)*SPS-V*CPS)
      BYGSW = -3.D0*YGSW*Q*(XGSW*SPS+ZGSW*CPS)
      BZGSW = Q*((P+T-2.D0*U)*CPS-V*SPS)
      RETURN
      END

C    *******************************************************************
c
      SUBROUTINE TS_SUN_08(IYEAR,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,
     *                     SDEC)
C     *PT*WARNING* Already double-precision
C
C    CALCULATES FOUR QUANTITIES NECESSARY FOR COORDINATE TRANSFORMATIONS
CWHICH DEPEND ON TS_SUN POSITION (AND, HENCE, ON UNIVERSAL TIME AND SEASON)
C
C     -------  INPUT PARAMETERS:
CIYR,IDAY,IHOUR,MIN,ISEC -  YEAR, DAY, AND UNIVERSAL TIME IN HOURS, MINUTES,
C     AND SECONDS  (IDAY=1 CORRESPONDS TO JANUARY 1).
C
C     -------  OUTPUT PARAMETERS:
C   GST - GREENWICH MEAN SIDEREAL TIME, SLONG - LONGITUDE ALONG ECLIPTIC
C  SRASN - RIGHT ASCENSION,  SDEC - DECLINATION  OF THE TS_SUN (RADIANS)
C     ORIGINAL VERSION OF THIS SUBROUTINE HAS BEEN COMPILED FROM:
C     RUSSELL, C.T., COSMIC ELECTRODYNAMICS, 1971, V.2, PP.184-196.
C
C     LAST MODIFICATION:  MARCH 31, 2003 (ONLY SOME NOTATION CHANGES)
C
C     ORIGINAL VERSION WRITTEN BY:    Gilbert D. Mead
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION GST,SDEC,SLONG,SRASN
      INTEGER IDAY,IHOUR,ISEC,IYEAR,MIN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION COSD,DJ,FDAY,G,OBLIQ,RAD,SC,SIND,SLP,SOB,T,VL
C     ..
C     .. External Functions ..
C      DOUBLE PRECISION DFLOAT
C      EXTERNAL DFLOAT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN,ATAN2,COS,DMOD,SIN,SQRT
C     ..
C     .. Data statements ..
      DATA RAD/57.295779513D0/
C     ..
C
      IF (IYEAR.LT.1901 .OR. IYEAR.GT.2099) RETURN
C     *PT*WARNING* Constant already double-precision
      FDAY = DBLE(IHOUR*3600+MIN*60+ISEC)/86400.D0
C     *PT*WARNING* Constant already double-precision
      DJ = 365*(IYEAR-1900) + (IYEAR-1901)/4 + IDAY - 0.5D0 + FDAY
      T = DJ/36525.D0
C     *PT*WARNING* Constant already double-precision
      VL = DMOD(279.696678D0+0.9856473354D0*DJ,360.D0)
C     *PT*WARNING* Constant already double-precision
      GST = DMOD(279.690983D0+.9856473354D0*DJ+360.D0*FDAY+180.D0,
     *      360.D0)/RAD
C     *PT*WARNING* Constant already double-precision
      G = DMOD(358.475845D0+0.985600267D0*DJ,360.D0)/RAD
      SLONG = (VL+(1.91946D0-0.004789D0*T)*SIN(G)+
     *        0.020094D0*SIN(2.D0*G))/RAD
      IF (SLONG.GT.6.2831853D0) SLONG = SLONG - 6.2831853D0
      IF (SLONG.LT.0.D0) SLONG = SLONG + 6.2831853D0
      OBLIQ = (23.45229D0-0.0130125D0*T)/RAD
      SOB = SIN(OBLIQ)
      SLP = SLONG - 9.924D-5
C
C    THE LAST CONSTANT IS A CORRECTION FOR THE ANGULAR ABERRATION DUE TO
C     EARTH'S ORBITAL MOTION
C
      SIND = SOB*SIN(SLP)
      COSD = SQRT(1.D0-SIND**2)
      SC = SIND/COSD
      SDEC = ATAN(SC)
      SRASN = 3.141592654D0 - ATAN2(COS(OBLIQ)/SOB*SC,-COS(SLP)/COSD)
      RETURN
      END
C
C================================================================================
c
      SUBROUTINE TS_SPHCAR_08(R,THETA,PHI,X,Y,Z,J)
C
C     CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICE VERSA
C     (THETA AND PHI IN RADIANS).
C
C                  J>0            J<0
C     -----INPUT:   J,R,THETA,PHI     J,X,Y,Z
C     ----OUTPUT:      X,Y,Z        R,THETA,PHI
C
C     NOTE: AT THE POLES (X=0 AND Y=0) WE ASSUME PHI=0 WHEN CONVERTING
C        FROM CARTESIAN TO SPHERICAL COORDS (I.E., FOR J<0)
C
C LAST MOFIFICATION:  APRIL 1, 2003 (ONLY SOME NOTATION CHANGES AND MORE
C                         COMMENTS ADDED)
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION PHI,R,THETA,X,Y,Z
      INTEGER J
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION SQ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,SIN,SQRT
C     ..
      IF (J.GT.0) GO TO 30
      SQ = X**2 + Y**2
      R = SQRT(SQ+Z**2)
      IF (SQ.NE.0.D0) GO TO 20
      PHI = 0.D0
      IF (Z.LT.0.D0) GO TO 10
      THETA = 0.D0
      RETURN
   10 THETA = 3.141592654D0
      RETURN
   20 SQ = SQRT(SQ)
      PHI = ATAN2(Y,X)
      THETA = ATAN2(SQ,Z)
      IF (PHI.LT.0.D0) PHI = PHI + 6.28318531D0
      RETURN
   30 SQ = R*SIN(THETA)
      X = SQ*COS(PHI)
      Y = SQ*SIN(PHI)
      Z = R*COS(THETA)
      RETURN
      END
C
C===========================================================================
c
      SUBROUTINE TS_BSPCAR_08(THETA,PHI,BR,BTHETA,BPHI,BX,BY,BZ)
C
C     CALCULATES CARTESIAN FIELD COMPONENTS FROM LOCAL SPHERICAL ONES
C
C     -----INPUT:   THETA,PHI - SPHERICAL ANGLES OF THE POINT IN RADIANS
C              BR,BTHETA,BPHI -  LOCAL SPHERICAL COMPONENTS OF THE FIELD
C     -----OUTPUT:  BX,BY,BZ - CARTESIAN COMPONENTS OF THE FIELD
C
C     LAST MOFIFICATION:  APRIL 1, 2003 (ONLY SOME NOTATION CHANGES)
C
C     WRITTEN BY:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BPHI,BR,BTHETA,BX,BY,BZ,PHI,THETA
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BE,C,CF,S,SF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC COS,SIN
C     ..
      S = SIN(THETA)
      C = COS(THETA)
      SF = SIN(PHI)
      CF = COS(PHI)
      BE = BR*S + BTHETA*C
      BX = BE*CF - BPHI*SF
      BY = BE*SF + BPHI*CF
      BZ = BR*C - BTHETA*S
      RETURN
      END
c
C==============================================================================
C
      SUBROUTINE BCARSP_08(X,Y,Z,BX,BY,BZ,BR,BTHETA,BPHI)
C
CALCULATES LOCAL SPHERICAL FIELD COMPONENTS FROM THOSE IN CARTESIAN SYSTEM
C
C     -----INPUT:   X,Y,Z  - CARTESIAN COMPONENTS OF THE POSITION VECTOR
C              BX,BY,BZ - CARTESIAN COMPONENTS OF THE FIELD VECTOR
C-----OUTPUT:  BR,BTHETA,BPHI - LOCAL SPHERICAL COMPONENTS OF THE FIELD VECTOR
C
C     NOTE: AT THE POLES (THETA=0 OR THETA=PI) WE ASSUME PHI=0,
C        AND HENCE BTHETA=BX, BPHI=BY
C
C     WRITTEN AND ADDED TO THIS PACKAGE:  APRIL 1, 2003,
C     AUTHOR:   N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BPHI,BR,BTHETA,BX,BY,BZ,X,Y,Z
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CPHI,CT,R,RHO,RHO2,SPHI,ST
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
      RHO2 = X**2 + Y**2
      R = SQRT(RHO2+Z**2)
      RHO = SQRT(RHO2)

      IF (RHO.NE.0.D0) THEN
         CPHI = X/RHO
         SPHI = Y/RHO
      ELSE
         CPHI = 1.D0
         SPHI = 0.D0
      END IF

      CT = Z/R
      ST = RHO/R

      BR = (X*BX+Y*BY+Z*BZ)/R
      BTHETA = (BX*CPHI+BY*SPHI)*CT - BZ*ST
      BPHI = BY*CPHI - BX*SPHI

      RETURN
      END
C
c=====================================================================================
C
      SUBROUTINE TS_RECALC_08(IYEAR,IDAY,IHOUR,MIN,ISEC,VGSEX,VGSEY,
     *                        VGSEZ)
C
C1. PREPARES ELEMENTS OF ROTATION MATRICES FOR TRANSFORMATIONS OF VECTORS BETWEEN
C     SEVERAL COORDINATE SYSTEMS, MOST FREQUENTLY USED IN SPACE PHYSICS.
C
C2. PREPARES COEFFICIENTS USED IN THE CALCULATION OF THE MAIN TS_GEOMAGNETIC FIELD
C      (IGRF MODEL)
C
CTHIS SUBROUTINE SHOULD BE INVOKED BEFORE USING THE FOLLOWING SUBROUTINES:
CIGRF_GEO_08, IGRF_GSW_08, DIP_08, TS_GEOMAG_08, GEOGSW, MAGSW_08, SMGSW_08, GSWGSE,
c     GEIGEO_08, TRACE, STEP_08, RHAND_08.
C
CTHERE IS NO NEED TO REPEATEDLY INVOKE TS_RECALC_08, IF MULTIPLE CALCULATIONS ARE MADE
C     FOR THE SAME DATE/TIME AND SOLAR WIND FLOW DIRECTION.
C
C     -----INPUT PARAMETERS:
C
C     IYEAR   -  YEAR NUMBER (FOUR DIGITS)
C     IDAY  -  DAY OF YEAR (DAY 1 = JAN 1)
C     IHOUR -  HOUR OF DAY (00 TO 23)
C     MIN   -  MINUTE OF HOUR (00 TO 59)
C     ISEC  -  SECONDS OF MINUTE (00 TO 59)
CVGSEX,VGSEY,VGSEZ - GSE (GEOCENTRIC SOLAR-ECLIPTIC) COMPONENTS OF THE OBSERVED
C                              SOLAR WIND FLOW VELOCITY (IN KM/S)
C
CIMPORTANT: IF ONLY QUESTIONABLE INFORMATION (OR NO INFORMATION AT ALL) IS AVAILABLE
C        ON THE SOLAR WIND SPEED, OR, IF THE STANDARD GSM AND/OR SM COORDINATES ARE
C        INTENDED TO BE USED, THEN SET VGSEX=-400.0 AND VGSEY=VGSEZ=0. IN THIS CASE,
C        THE GSW COORDINATE SYSTEM BECOMES IDENTICAL TO THE STANDARD GSM.
C
C        IF ONLY SCALAR SPEED V OF THE SOLAR WIND IS KNOWN, THEN SETTING
C        VGSEX=-V, VGSEY=29.78, VGSEZ=0.0 WILL TAKE INTO ACCOUNT THE ~4 degs
C          ABERRATION OF THE MAGNETOSPHERE DUE TO EARTH'S ORBITAL MOTION
C
C        IF ALL THREE GSE COMPONENTS OF THE SOLAR WIND VELOCITY ARE AVAILABLE,
C        PLEASE NOTE THAT IN SOME SOLAR WIND DATABASES THE ABERRATION EFFECT
C        HAS ALREADY BEEN TAKEN INTO ACCOUNT BY SUBTRACTING 29.78 KM/S FROM VYGSE;
C        IN THAT CASE, THE UNABERRATED (OBSERVED) VYGSE VALUES SHOULD BE RESTORED
C        BY ADDING BACK THE 29.78 KM/S CORRECTION. WHETHER OR NOT TO DO THAT, MUST
C        BE EITHER VERIFIED WITH THE DATA ORIGINATOR OR DETERMINED BY AVERAGING
C             VGSEY OVER A SUFFICIENTLY LONG TIME INTERVAL.
C
C     -----OUTPUT PARAMETERS:  NONE (ALL OUTPUT QUANTITIES ARE PLACED
C                      INTO THE COMMON BLOCKS /GEOPACK1/ AND /GEOPACK2/)
C
C     OTHER SUBROUTINES CALLED BY THIS ONE: TS_SUN_08
C
C     AUTHOR:  N.A. TSYGANENKO
C     DATE:    DEC.1, 1991
C
C     REVISION OF NOVEMBER 30, 2010:
C
CThe table of IGRF coefficients was extended to include those for the epoch 2010
c        (for details, see http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html)
C
CREVISION OF NOVEMBER 15, 2007: ADDED THE POSSIBILITY TO TAKE INTO ACCOUNT THE OBSERVED
CDEFLECTION OF THE SOLAR WIND FLOW FROM STRICTLY RADIAL DIRECTION. TO THAT END, THREE
CGSE COMPONENTS OF THE SOLAR WIND VELOCITY WERE ADDED TO THE INPUT PARAMETERS.
C
c CORRECTION OF MAY 9, 2006:  INTERPOLATION OF THE COEFFICIENTS (BETWEEN
C  LABELS 50 AND 105) IS NOW MADE THROUGH THE LAST ELEMENT OF THE ARRAYS
C   G(105)  AND H(105) (PREVIOUSLY MADE ONLY THROUGH N=66, WHICH IN SOME
C     CASES CAUSED RUNTIME ERRORS)
c
C     REVISION OF MAY 3, 2005:
CThe table of IGRF coefficients was extended to include those for the epoch 2005
c   the maximal order of spherical harmonics was also increased up to 13
c        (for details, see http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html)
c
C     REVISION OF APRIL 3, 2003:
cThe code now includes preparation of the model coefficients for the subroutines
cIGRF_08 and TS_GEOMAG_08. This eliminates the need for the SAVE statements, used
cin the old versions, making the codes easier and more compiler-independent.
C
C
C
CTHE COMMON BLOCK /GEOPACK1/ CONTAINS ELEMENTS OF THE ROTATION MATRICES AND OTHER
CPARAMETERS RELATED TO THE COORDINATE TRANSFORMATIONS PERFORMED BY THIS PACKAGE
C
C
CTHE COMMON BLOCK /GEOPACK2/ CONTAINS COEFFICIENTS OF THE IGRF FIELD MODEL, CALCULATED
CFOR A GIVEN YEAR AND DAY FROM THEIR STANDARD EPOCH VALUES. THE ARRAY REC CONTAINS
CCOEFFICIENTS USED IN THE RECURSION RELATIONS FOR LEGENDRE ASSOCIATE POLYNOMIALS.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION VGSEX,VGSEY,VGSEZ
      INTEGER IDAY,IHOUR,ISEC,IYEAR,MIN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AA,DIP1,DIP2,DIP3,DJ,DT,DX1,DX2,DX3,DY1,DY2,DY3,
     *                 DZ1,DZ2,DZ3,EXMAGX,EXMAGY,EXMAGZ,EYMAGX,EYMAGY,
     *                 F1,F2,GST,G_10,G_11,H_11,OBLIQ,P,S,S1,S2,S3,SDEC,
     *                 SLONG,SQ,SQQ,SQR,SRASN,T,V,X1,X2,X3,Y,Y1,Y2,Y3,
     *                 Z1,Z2,Z3
      INTEGER ISW,IY,M,MN,MNN,N,N2
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION DG10(45),DH10(45),G00(105),G05(105),G10(105),
     *                 G65(105),G70(105),G75(105),G80(105),G85(105),
     *                 G90(105),G95(105),H00(105),H05(105),H10(105),
     *                 H65(105),H70(105),H75(105),H80(105),H85(105),
     *                 H90(105),H95(105)
C     ..
C     .. External Subroutines ..
      EXTERNAL TS_SUN_08
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ASIN,COS,DBLE,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,SFI,CFI,SPS,
     *       CPS,DS3,CGST,SGST,PSI,A11,A21,A31,A12,A22,A32,A13,A23,A33,
     *       E11,E21,E31,E12,E22,E32,E13,E23,E33
      COMMON /GEOPACK2/G,H,REC
      DOUBLE PRECISION A11,A12,A13,A21,A22,A23,A31,A32,A33,CFI,CGST,CL0,
     *                 CPS,CT0,CTCL,CTSL,DS3,E11,E12,E13,E21,E22,E23,
     *                 E31,E32,E33,PSI,SFI,SGST,SL0,SPS,ST0,STCL,STSL
      DOUBLE PRECISION G(105),H(105),REC(105)
C     added by B. Rideout to allow time to be passed to my igrf call
      COMMON /BR_FYEAR/FYEAR
      DOUBLE PRECISION FYEAR
C     ..
C     .. Save statement ..
      SAVE ISW
C     ..
C     .. Data statements ..
C
c
c
c
c
c
c
C
C
C
C
C
C
C
C
C
C
C
      DATA ISW/0/
      DATA G65/0.D0,-30334.D0,-2119.D0,-1662.D0,2997.D0,1594.D0,1297.D0,
     *     -2038.D0,1292.D0,856.D0,957.D0,804.D0,479.D0,-390.D0,252.D0,
     *     -219.D0,358.D0,254.D0,-31.D0,-157.D0,-62.D0,45.D0,61.D0,8.D0,
     *     -228.D0,4.D0,1.D0,-111.D0,75.D0,-57.D0,4.D0,13.D0,-26.D0,
     *     -6.D0,13.D0,1.D0,13.D0,5.D0,-4.D0,-14.D0,0.D0,8.D0,-1.D0,
     *     11.D0,4.D0,8.D0,10.D0,2.D0,-13.D0,10.D0,-1.D0,-1.D0,5.D0,
     *     1.D0,-2.D0,-2.D0,-3.D0,2.D0,-5.D0,-2.D0,4.D0,4.D0,0.D0,2.D0,
     *     2.D0,0.D0,39*0.D0/
      DATA H65/0.D0,0.D0,5776.D0,0.D0,-2016.D0,114.D0,0.D0,-404.D0,
     *     240.D0,-165.D0,0.D0,148.D0,-269.D0,13.D0,-269.D0,0.D0,19.D0,
     *     128.D0,-126.D0,-97.D0,81.D0,0.D0,-11.D0,100.D0,68.D0,-32.D0,
     *     -8.D0,-7.D0,0.D0,-61.D0,-27.D0,-2.D0,6.D0,26.D0,-23.D0,
     *     -12.D0,0.D0,7.D0,-12.D0,9.D0,-16.D0,4.D0,24.D0,-3.D0,-17.D0,
     *     0.D0,-22.D0,15.D0,7.D0,-4.D0,-5.D0,10.D0,10.D0,-4.D0,1.D0,
     *     0.D0,2.D0,1.D0,2.D0,6.D0,-4.D0,0.D0,-2.D0,3.D0,0.D0,-6.D0,
     *     39*0.D0/
      DATA G70/0.D0,-30220.D0,-2068.D0,-1781.D0,3000.D0,1611.D0,1287.D0,
     *     -2091.D0,1278.D0,838.D0,952.D0,800.D0,461.D0,-395.D0,234.D0,
     *     -216.D0,359.D0,262.D0,-42.D0,-160.D0,-56.D0,43.D0,64.D0,
     *     15.D0,-212.D0,2.D0,3.D0,-112.D0,72.D0,-57.D0,1.D0,14.D0,
     *     -22.D0,-2.D0,13.D0,-2.D0,14.D0,6.D0,-2.D0,-13.D0,-3.D0,5.D0,
     *     0.D0,11.D0,3.D0,8.D0,10.D0,2.D0,-12.D0,10.D0,-1.D0,0.D0,3.D0,
     *     1.D0,-1.D0,-3.D0,-3.D0,2.D0,-5.D0,-1.D0,6.D0,4.D0,1.D0,0.D0,
     *     3.D0,-1.D0,39*0.D0/
      DATA H70/0.D0,0.D0,5737.D0,0.D0,-2047.D0,25.D0,0.D0,-366.D0,
     *     251.D0,-196.D0,0.D0,167.D0,-266.D0,26.D0,-279.D0,0.D0,26.D0,
     *     139.D0,-139.D0,-91.D0,83.D0,0.D0,-12.D0,100.D0,72.D0,-37.D0,
     *     -6.D0,1.D0,0.D0,-70.D0,-27.D0,-4.D0,8.D0,23.D0,-23.D0,-11.D0,
     *     0.D0,7.D0,-15.D0,6.D0,-17.D0,6.D0,21.D0,-6.D0,-16.D0,0.D0,
     *     -21.D0,16.D0,6.D0,-4.D0,-5.D0,10.D0,11.D0,-2.D0,1.D0,0.D0,
     *     1.D0,1.D0,3.D0,4.D0,-4.D0,0.D0,-1.D0,3.D0,1.D0,-4.D0,39*0.D0/
      DATA G75/0.D0,-30100.D0,-2013.D0,-1902.D0,3010.D0,1632.D0,1276.D0,
     *     -2144.D0,1260.D0,830.D0,946.D0,791.D0,438.D0,-405.D0,216.D0,
     *     -218.D0,356.D0,264.D0,-59.D0,-159.D0,-49.D0,45.D0,66.D0,
     *     28.D0,-198.D0,1.D0,6.D0,-111.D0,71.D0,-56.D0,1.D0,16.D0,
     *     -14.D0,0.D0,12.D0,-5.D0,14.D0,6.D0,-1.D0,-12.D0,-8.D0,4.D0,
     *     0.D0,10.D0,1.D0,7.D0,10.D0,2.D0,-12.D0,10.D0,-1.D0,-1.D0,
     *     4.D0,1.D0,-2.D0,-3.D0,-3.D0,2.D0,-5.D0,-2.D0,5.D0,4.D0,1.D0,
     *     0.D0,3.D0,-1.D0,39*0.D0/
      DATA H75/0.D0,0.D0,5675.D0,0.D0,-2067.D0,-68.D0,0.D0,-333.D0,
     *     262.D0,-223.D0,0.D0,191.D0,-265.D0,39.D0,-288.D0,0.D0,31.D0,
     *     148.D0,-152.D0,-83.D0,88.D0,0.D0,-13.D0,99.D0,75.D0,-41.D0,
     *     -4.D0,11.D0,0.D0,-77.D0,-26.D0,-5.D0,10.D0,22.D0,-23.D0,
     *     -12.D0,0.D0,6.D0,-16.D0,4.D0,-19.D0,6.D0,18.D0,-10.D0,-17.D0,
     *     0.D0,-21.D0,16.D0,7.D0,-4.D0,-5.D0,10.D0,11.D0,-3.D0,1.D0,
     *     0.D0,1.D0,1.D0,3.D0,4.D0,-4.D0,-1.D0,-1.D0,3.D0,1.D0,-5.D0,
     *     39*0.D0/
      DATA G80/0.D0,-29992.D0,-1956.D0,-1997.D0,3027.D0,1663.D0,1281.D0,
     *     -2180.D0,1251.D0,833.D0,938.D0,782.D0,398.D0,-419.D0,199.D0,
     *     -218.D0,357.D0,261.D0,-74.D0,-162.D0,-48.D0,48.D0,66.D0,
     *     42.D0,-192.D0,4.D0,14.D0,-108.D0,72.D0,-59.D0,2.D0,21.D0,
     *     -12.D0,1.D0,11.D0,-2.D0,18.D0,6.D0,0.D0,-11.D0,-7.D0,4.D0,
     *     3.D0,6.D0,-1.D0,5.D0,10.D0,1.D0,-12.D0,9.D0,-3.D0,-1.D0,7.D0,
     *     2.D0,-5.D0,-4.D0,-4.D0,2.D0,-5.D0,-2.D0,5.D0,3.D0,1.D0,2.D0,
     *     3.D0,0.D0,39*0.D0/
      DATA H80/0.D0,0.D0,5604.D0,0.D0,-2129.D0,-200.D0,0.D0,-336.D0,
     *     271.D0,-252.D0,0.D0,212.D0,-257.D0,53.D0,-297.D0,0.D0,46.D0,
     *     150.D0,-151.D0,-78.D0,92.D0,0.D0,-15.D0,93.D0,71.D0,-43.D0,
     *     -2.D0,17.D0,0.D0,-82.D0,-27.D0,-5.D0,16.D0,18.D0,-23.D0,
     *     -10.D0,0.D0,7.D0,-18.D0,4.D0,-22.D0,9.D0,16.D0,-13.D0,-15.D0,
     *     0.D0,-21.D0,16.D0,9.D0,-5.D0,-6.D0,9.D0,10.D0,-6.D0,2.D0,
     *     0.D0,1.D0,0.D0,3.D0,6.D0,-4.D0,0.D0,-1.D0,4.D0,0.D0,-6.D0,
     *     39*0.D0/
      DATA G85/0.D0,-29873.D0,-1905.D0,-2072.D0,3044.D0,1687.D0,1296.D0,
     *     -2208.D0,1247.D0,829.D0,936.D0,780.D0,361.D0,-424.D0,170.D0,
     *     -214.D0,355.D0,253.D0,-93.D0,-164.D0,-46.D0,53.D0,65.D0,
     *     51.D0,-185.D0,4.D0,16.D0,-102.D0,74.D0,-62.D0,3.D0,24.D0,
     *     -6.D0,4.D0,10.D0,0.D0,21.D0,6.D0,0.D0,-11.D0,-9.D0,4.D0,4.D0,
     *     4.D0,-4.D0,5.D0,10.D0,1.D0,-12.D0,9.D0,-3.D0,-1.D0,7.D0,1.D0,
     *     -5.D0,-4.D0,-4.D0,3.D0,-5.D0,-2.D0,5.D0,3.D0,1.D0,2.D0,3.D0,
     *     0.D0,39*0.D0/
      DATA H85/0.D0,0.D0,5500.D0,0.D0,-2197.D0,-306.D0,0.D0,-310.D0,
     *     284.D0,-297.D0,0.D0,232.D0,-249.D0,69.D0,-297.D0,0.D0,47.D0,
     *     150.D0,-154.D0,-75.D0,95.D0,0.D0,-16.D0,88.D0,69.D0,-48.D0,
     *     -1.D0,21.D0,0.D0,-83.D0,-27.D0,-2.D0,20.D0,17.D0,-23.D0,
     *     -7.D0,0.D0,8.D0,-19.D0,5.D0,-23.D0,11.D0,14.D0,-15.D0,-11.D0,
     *     0.D0,-21.D0,15.D0,9.D0,-6.D0,-6.D0,9.D0,9.D0,-7.D0,2.D0,0.D0,
     *     1.D0,0.D0,3.D0,6.D0,-4.D0,0.D0,-1.D0,4.D0,0.D0,-6.D0,39*0.D0/
      DATA G90/0.D0,-29775.D0,-1848.D0,-2131.D0,3059.D0,1686.D0,1314.D0,
     *     -2239.D0,1248.D0,802.D0,939.D0,780.D0,325.D0,-423.D0,141.D0,
     *     -214.D0,353.D0,245.D0,-109.D0,-165.D0,-36.D0,61.D0,65.D0,
     *     59.D0,-178.D0,3.D0,18.D0,-96.D0,77.D0,-64.D0,2.D0,26.D0,
     *     -1.D0,5.D0,9.D0,0.D0,23.D0,5.D0,-1.D0,-10.D0,-12.D0,3.D0,
     *     4.D0,2.D0,-6.D0,4.D0,9.D0,1.D0,-12.D0,9.D0,-4.D0,-2.D0,7.D0,
     *     1.D0,-6.D0,-3.D0,-4.D0,2.D0,-5.D0,-2.D0,4.D0,3.D0,1.D0,3.D0,
     *     3.D0,0.D0,39*0.D0/
      DATA H90/0.D0,0.D0,5406.D0,0.D0,-2279.D0,-373.D0,0.D0,-284.D0,
     *     293.D0,-352.D0,0.D0,247.D0,-240.D0,84.D0,-299.D0,0.D0,46.D0,
     *     154.D0,-153.D0,-69.D0,97.D0,0.D0,-16.D0,82.D0,69.D0,-52.D0,
     *     1.D0,24.D0,0.D0,-80.D0,-26.D0,0.D0,21.D0,17.D0,-23.D0,-4.D0,
     *     0.D0,10.D0,-19.D0,6.D0,-22.D0,12.D0,12.D0,-16.D0,-10.D0,0.D0,
     *     -20.D0,15.D0,11.D0,-7.D0,-7.D0,9.D0,8.D0,-7.D0,2.D0,0.D0,
     *     2.D0,1.D0,3.D0,6.D0,-4.D0,0.D0,-2.D0,3.D0,-1.D0,-6.D0,
     *     39*0.D0/
      DATA G95/0.D0,-29692.D0,-1784.D0,-2200.D0,3070.D0,1681.D0,1335.D0,
     *     -2267.D0,1249.D0,759.D0,940.D0,780.D0,290.D0,-418.D0,122.D0,
     *     -214.D0,352.D0,235.D0,-118.D0,-166.D0,-17.D0,68.D0,67.D0,
     *     68.D0,-170.D0,-1.D0,19.D0,-93.D0,77.D0,-72.D0,1.D0,28.D0,
     *     5.D0,4.D0,8.D0,-2.D0,25.D0,6.D0,-6.D0,-9.D0,-14.D0,9.D0,6.D0,
     *     -5.D0,-7.D0,4.D0,9.D0,3.D0,-10.D0,8.D0,-8.D0,-1.D0,10.D0,
     *     -2.D0,-8.D0,-3.D0,-6.D0,2.D0,-4.D0,-1.D0,4.D0,2.D0,2.D0,5.D0,
     *     1.D0,0.D0,39*0.D0/
      DATA H95/0.D0,0.D0,5306.D0,0.D0,-2366.D0,-413.D0,0.D0,-262.D0,
     *     302.D0,-427.D0,0.D0,262.D0,-236.D0,97.D0,-306.D0,0.D0,46.D0,
     *     165.D0,-143.D0,-55.D0,107.D0,0.D0,-17.D0,72.D0,67.D0,-58.D0,
     *     1.D0,36.D0,0.D0,-69.D0,-25.D0,4.D0,24.D0,17.D0,-24.D0,-6.D0,
     *     0.D0,11.D0,-21.D0,8.D0,-23.D0,15.D0,11.D0,-16.D0,-4.D0,0.D0,
     *     -20.D0,15.D0,12.D0,-6.D0,-8.D0,8.D0,5.D0,-8.D0,3.D0,0.D0,
     *     1.D0,0.D0,4.D0,5.D0,-5.D0,-1.D0,-2.D0,1.D0,-2.D0,-7.D0,
     *     39*0.D0/
      DATA G00/0.D0,-29619.4D0,-1728.2D0,-2267.7D0,3068.4D0,1670.9D0,
     *     1339.6D0,-2288.D0,1252.1D0,714.5D0,932.3D0,786.8D0,250.D0,
     *     -403.D0,111.3D0,-218.8D0,351.4D0,222.3D0,-130.4D0,-168.6D0,
     *     -12.9D0,72.3D0,68.2D0,74.2D0,-160.9D0,-5.9D0,16.9D0,-90.4D0,
     *     79.0D0,-74.0D0,0.D0,33.3D0,9.1D0,6.9D0,7.3D0,-1.2D0,24.4D0,
     *     6.6D0,-9.2D0,-7.9D0,-16.6D0,9.1D0,7.0D0,-7.9D0,-7.D0,5.D0,
     *     9.4D0,3.D0,-8.4D0,6.3D0,-8.9D0,-1.5D0,9.3D0,-4.3D0,-8.2D0,
     *     -2.6D0,-6.D0,1.7D0,-3.1D0,-0.5D0,3.7D0,1.D0,2.D0,4.2D0,0.3D0,
     *     -1.1D0,2.7D0,-1.7D0,-1.9D0,1.5D0,-0.1D0,0.1D0,-0.7D0,0.7D0,
     *     1.7D0,0.1D0,1.2D0,4.0D0,-2.2D0,-0.3D0,0.2D0,0.9D0,-0.2D0,
     *     0.9D0,-0.5D0,0.3D0,-0.3D0,-0.4D0,-0.1D0,-0.2D0,-0.4D0,-0.2D0,
     *     -0.9D0,0.3D0,0.1D0,-0.4D0,1.3D0,-0.4D0,0.7D0,-0.4D0,0.3D0,
     *     -0.1D0,0.4D0,0.D0,0.1D0/
      DATA H00/0.D0,0.D0,5186.1D0,0.D0,-2481.6D0,-458.0D0,0.D0,-227.6D0,
     *     293.4D0,-491.1D0,0.D0,272.6D0,-231.9D0,119.8D0,-303.8D0,0.D0,
     *     43.8D0,171.9D0,-133.1D0,-39.3D0,106.3D0,0.D0,-17.4D0,63.7D0,
     *     65.1D0,-61.2D0,0.7D0,43.8D0,0.D0,-64.6D0,-24.2D0,6.2D0,24.D0,
     *     14.8D0,-25.4D0,-5.8D0,0.0D0,11.9D0,-21.5D0,8.5D0,-21.5D0,
     *     15.5D0,8.9D0,-14.9D0,-2.1D0,0.0D0,-19.7D0,13.4D0,12.5D0,
     *     -6.2D0,-8.4D0,8.4D0,3.8D0,-8.2D0,4.8D0,0.0D0,1.7D0,0.0D0,
     *     4.0D0,4.9D0,-5.9D0,-1.2D0,-2.9D0,0.2D0,-2.2D0,-7.4D0,0.0D0,
     *     0.1D0,1.3D0,-0.9D0,-2.6D0,0.9D0,-0.7D0,-2.8D0,-0.9D0,-1.2D0,
     *     -1.9D0,-0.9D0,0.0D0,-0.4D0,0.3D0,2.5D0,-2.6D0,0.7D0,0.3D0,
     *     0.0D0,0.0D0,0.3D0,-0.9D0,-0.4D0,0.8D0,0.0D0,-0.9D0,0.2D0,
     *     1.8D0,-0.4D0,-1.0D0,-0.1D0,0.7D0,0.3D0,0.6D0,0.3D0,-0.2D0,
     *     -0.5D0,-0.9D0/
      DATA G05/0.D0,-29554.6D0,-1669.0D0,-2337.2D0,3047.7D0,1657.8D0,
     *     1336.3D0,-2305.8D0,1246.4D0,672.5D0,920.6D0,798.0D0,210.7D0,
     *     -379.9D0,100.0D0,-227.0D0,354.4D0,208.9D0,-136.5D0,-168.1D0,
     *     -13.6D0,73.6D0,69.6D0,76.7D0,-151.3D0,-14.6D0,14.6D0,-86.4D0,
     *     79.9D0,-74.5D0,-1.7D0,38.7D0,12.3D0,9.4D0,5.4D0,1.9D0,24.8D0,
     *     7.6D0,-11.7D0,-6.9D0,-18.1D0,10.2D0,9.4D0,-11.3D0,-4.9D0,
     *     5.6D0,9.8D0,3.6D0,-6.9D0,5.0D0,-10.8D0,-1.3D0,8.8D0,-6.7D0,
     *     -9.2D0,-2.2D0,-6.1D0,1.4D0,-2.4D0,-0.2D0,3.1D0,0.3D0,2.1D0,
     *     3.8D0,-0.2D0,-2.1D0,2.9D0,-1.6D0,-1.9D0,1.4D0,-0.3D0,0.3D0,
     *     -0.8D0,0.5D0,1.8D0,0.2D0,1.0D0,4.0D0,-2.2D0,-0.3D0,0.2D0,
     *     0.9D0,-0.4D0,1.0D0,-0.3D0,0.5D0,-0.4D0,-0.4D0,0.1D0,-0.5D0,
     *     -0.1D0,-0.2D0,-0.9D0,0.3D0,0.3D0,-0.4D0,1.2D0,-0.4D0,0.8D0,
     *     -0.3D0,0.4D0,-0.1D0,0.4D0,-0.1D0,-0.2D0/
      DATA H05/0.D0,0.0D0,5078.0D0,0.0D0,-2594.5D0,-515.4D0,0.0D0,
     *     -198.9D0,269.7D0,-524.7D0,0.0D0,282.1D0,-225.2D0,145.2D0,
     *     -305.4D0,0.0D0,42.7D0,180.3D0,-123.5D0,-19.6D0,103.9D0,0.0D0,
     *     -20.3D0,54.8D0,63.6D0,-63.5D0,0.2D0,50.9D0,0.0D0,-61.1D0,
     *     -22.6D0,6.8D0,25.4D0,10.9D0,-26.3D0,-4.6D0,0.0D0,11.2D0,
     *     -20.9D0,9.8D0,-19.7D0,16.2D0,7.6D0,-12.8D0,-0.1D0,0.0D0,
     *     -20.1D0,12.7D0,12.7D0,-6.7D0,-8.2D0,8.1D0,2.9D0,-7.7D0,6.0D0,
     *     0.0D0,2.2D0,0.1D0,4.5D0,4.8D0,-6.7D0,-1.0D0,-3.5D0,-0.9D0,
     *     -2.3D0,-7.9D0,0.0D0,0.3D0,1.4D0,-0.8D0,-2.3D0,0.9D0,-0.6D0,
     *     -2.7D0,-1.1D0,-1.6D0,-1.9D0,-1.4D0,0.0D0,-0.6D0,0.2D0,2.4D0,
     *     -2.6D0,0.6D0,0.4D0,0.0D0,0.0D0,0.3D0,-0.9D0,-0.3D0,0.9D0,
     *     0.0D0,-0.8D0,0.3D0,1.7D0,-0.5D0,-1.1D0,0.0D0,0.6D0,0.2D0,
     *     0.5D0,0.4D0,-0.2D0,-0.6D0,-0.9D0/
      DATA G10/0.D0,-29496.5D0,-1585.9D0,-2396.6D0,3026.0D0,1668.6D0,
     *     1339.7D0,-2326.3D0,1231.7D0,634.2D0,912.6D0,809.0D0,166.6D0,
     *     -357.1D0,89.7D0,-231.1D0,357.2D0,200.3D0,-141.2D0,-163.1D0,
     *     -7.7D0,72.8D0,68.6D0,76.0D0,-141.4D0,-22.9D0,13.1D0,-77.9D0,
     *     80.4D0,-75.0D0,-4.7D0,45.3D0,14.0D0,10.4D0,1.6D0,4.9D0,
     *     24.3D0,8.2D0,-14.5D0,-5.7D0,-19.3D0,11.6D0,10.9D0,-14.1D0,
     *     -3.7D0,5.4D0,9.4D0,3.4D0,-5.3D0,3.1D0,-12.4D0,-0.8D0,8.4D0,
     *     -8.4D0,-10.1D0,-2.0D0,-6.3D0,0.9D0,-1.1D0,-0.2D0,2.5D0,
     *     -0.3D0,2.2D0,3.1D0,-1.0D0,-2.8D0,3.0D0,-1.5D0,-2.1D0,1.6D0,
     *     -0.5D0,0.5D0,-0.8D0,0.4D0,1.8D0,0.2D0,0.8D0,3.8D0,-2.1D0,
     *     -0.2D0,0.3D0,1.0D0,-0.7D0,0.9D0,-0.1D0,0.5D0,-0.4D0,-0.4D0,
     *     0.2D0,-0.8D0,0.0D0,-0.2D0,-0.9D0,0.3D0,0.4D0,-0.4D0,1.1D0,
     *     -0.3D0,0.8D0,-0.2D0,0.4D0,0.0D0,0.4D0,-0.3D0,-0.3D0/
      DATA H10/0.0D0,0.0D0,4945.1D0,0.0D0,-2707.7D0,-575.4D0,0.0D0,
     *     -160.5D0,251.7D0,-536.8D0,0.0D0,286.4D0,-211.2D0,164.4D0,
     *     -309.2D0,0.0D0,44.7D0,188.9D0,-118.1D0,0.1D0,100.9D0,0.0D0,
     *     -20.8D0,44.2D0,61.5D0,-66.3D0,3.1D0,54.9D0,0.0D0,-57.8D0,
     *     -21.2D0,6.6D0,24.9D0,7.0D0,-27.7D0,-3.4D0,0.0D0,10.9D0,
     *     -20.0D0,11.9D0,-17.4D0,16.7D0,7.1D0,-10.8D0,1.7D0,0.0D0,
     *     -20.5D0,11.6D0,12.8D0,-7.2D0,-7.4D0,8.0D0,2.2D0,-6.1D0,7.0D0,
     *     0.0D0,2.8D0,-0.1D0,4.7D0,4.4D0,-7.2D0,-1.0D0,-4.0D0,-2.0D0,
     *     -2.0D0,-8.3D0,0.0D0,0.1D0,1.7D0,-0.6D0,-1.8D0,0.9D0,-0.4D0,
     *     -2.5D0,-1.3D0,-2.1D0,-1.9D0,-1.8D0,0.0D0,-0.8D0,0.3D0,2.2D0,
     *     -2.5D0,0.5D0,0.6D0,0.0D0,0.1D0,0.3D0,-0.9D0,-0.2D0,0.8D0,
     *     0.0D0,-0.8D0,0.3D0,1.7D0,-0.6D0,-1.2D0,-0.1D0,0.5D0,0.1D0,
     *     0.5D0,0.4D0,-0.2D0,-0.5D0,-0.8D0/
      DATA DG10/0.0D0,11.4D0,16.7D0,-11.3D0,-3.9D0,2.7D0,1.3D0,-3.9D0,
     *     -2.9D0,-8.1D0,-1.4D0,2.0D0,-8.9D0,4.4D0,-2.3D0,-0.5D0,0.5D0,
     *     -1.5D0,-0.7D0,1.3D0,1.4D0,-0.3D0,-0.3D0,-0.3D0,1.9D0,-1.6D0,
     *     -0.2D0,1.8D0,0.2D0,-0.1D0,-0.6D0,1.4D0,0.3D0,0.1D0,-0.8D0,
     *     0.4D0,-0.1D0,0.1D0,-0.5D0,0.3D0,-0.3D0,0.3D0,0.2D0,-0.5D0,
     *     0.2D0/
      DATA DH10/0.0D0,0.0D0,-28.8D0,0.0D0,-23.0D0,-12.9D0,0.0D0,8.6D0,
     *     -2.9D0,-2.1D0,0.0D0,0.4D0,3.2D0,3.6D0,-0.8D0,0.0D0,0.5D0,
     *     1.5D0,0.9D0,3.7D0,-0.6D0,0.0D0,-0.1D0,-2.1D0,-0.4D0,-0.5D0,
     *     0.8D0,0.5D0,0.0D0,0.6D0,0.3D0,-0.2D0,-0.1D0,-0.8D0,-0.3D0,
     *     0.2D0,0.0D0,0.0D0,0.2D0,0.5D0,0.4D0,0.1D0,-0.1D0,0.4D0,0.4D0/
C     ..
C
      IF (VGSEY.EQ.0.D0 .AND. VGSEZ.EQ.0.D0 .AND. ISW.NE.1) THEN
C        PRINT *, ''
C        PRINT *,
C      *' TS_RECALC_08: RADIAL SOLAR WIND --> GSW SYSTEM IDENTICAL HERE'
C        PRINT *,
C *' TO STANDARD GSM (I.E., XGSW AXIS COINCIDES WITH EARTH-TS_SUN LINE)'
C        PRINT *, ''
         ISW = 1
      END IF

      IF ((VGSEY.NE.0.D0.OR.VGSEZ.NE.0.D0) .AND. ISW.NE.2) THEN
C        PRINT *, ''
C        PRINT *,
C     *' WARNING: NON-RADIAL SOLAR WIND FLOW SPECIFIED IN TS_RECALC_08;'
C        PRINT *,
C    *' HENCE XGSW AXIS IS ASSUMED ORIENTED ANTIPARALLEL TO V_SW VECTOR'
C        PRINT *, ''
         ISW = 2
      END IF
C
      IY = IYEAR
      FYEAR = DBLE(IYEAR) + (IDAY-1)/366.0
C
CWE ARE RESTRICTED BY THE INTERVAL 1965-2010, FOR WHICH THE IGRF COEFFICIENTS
cARE KNOWN; IF IYEAR IS OUTSIDE THIS INTERVAL, THEN THE SUBROUTINE USES THE
C      NEAREST LIMITING VALUE AND PRINTS A WARNING:
C
      IF (IY.LT.1965) THEN
         IY = 1965
C         WRITE (*,FMT=9000) IYEAR,IY
      END IF

      IF (IY.GT.2015) THEN
         IY = 2015
C         WRITE (*,FMT=9000) IYEAR,IY
      END IF

C
CCALCULATE THE ARRAY REC, CONTAINING COEFFICIENTS FOR THE RECURSION RELATIONS,
CUSED IN THE IGRF SUBROUTINE FOR CALCULATING THE ASSOCIATE LEGENDRE POLYNOMIALS
C     AND THEIR DERIVATIVES:
c
      DO 20 N = 1,14
         N2 = 2*N - 1
         N2 = N2*(N2-2)
         DO 10 M = 1,N
            MN = N*(N-1)/2 + M
            REC(MN) = DBLE((N-M)*(N+M-2))/DBLE(N2)
   10    CONTINUE
   20 CONTINUE
C
      IF (IY.LT.1970) GO TO 40
      IF (IY.LT.1975) GO TO 60
      IF (IY.LT.1980) GO TO 80
      IF (IY.LT.1985) GO TO 100
      IF (IY.LT.1990) GO TO 120
      IF (IY.LT.1995) GO TO 140
      IF (IY.LT.2000) GO TO 160
      IF (IY.LT.2005) GO TO 180
      IF (IY.LT.2010) GO TO 200
C
C       EXTRAPOLATE BEYOND 2010:
C
      DT = DBLE(IY) + DBLE(IDAY-1)/365.25D0 - 2010.D0
      DO 30 N = 1,105
         G(N) = G10(N)
         H(N) = H10(N)
         IF (N.GT.45) GO TO 30
         G(N) = G(N) + DG10(N)*DT
         H(N) = H(N) + DH10(N)*DT
   30 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEEN 1965 - 1970:
C
   40 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1965)/5.D0
      F1 = 1.D0 - F2
      DO 50 N = 1,105
         G(N) = G65(N)*F1 + G70(N)*F2
         H(N) = H65(N)*F1 + H70(N)*F2
   50 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 1970 - 1975:
C
   60 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1970)/5.D0
      F1 = 1.D0 - F2
      DO 70 N = 1,105
         G(N) = G70(N)*F1 + G75(N)*F2
         H(N) = H70(N)*F1 + H75(N)*F2
   70 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 1975 - 1980:
C
   80 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1975)/5.D0
      F1 = 1.D0 - F2
      DO 90 N = 1,105
         G(N) = G75(N)*F1 + G80(N)*F2
         H(N) = H75(N)*F1 + H80(N)*F2
   90 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 1980 - 1985:
C
  100 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1980)/5.D0
      F1 = 1.D0 - F2
      DO 110 N = 1,105
         G(N) = G80(N)*F1 + G85(N)*F2
         H(N) = H80(N)*F1 + H85(N)*F2
  110 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 1985 - 1990:
C
  120 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1985)/5.D0
      F1 = 1.D0 - F2
      DO 130 N = 1,105
         G(N) = G85(N)*F1 + G90(N)*F2
         H(N) = H85(N)*F1 + H90(N)*F2
  130 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 1990 - 1995:
C
  140 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1990)/5.D0
      F1 = 1.D0 - F2
      DO 150 N = 1,105
         G(N) = G90(N)*F1 + G95(N)*F2
         H(N) = H90(N)*F1 + H95(N)*F2
  150 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 1995 - 2000:
C
  160 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1995)/5.D0
      F1 = 1.D0 - F2
      DO 170 N = 1,105
         G(N) = G95(N)*F1 + G00(N)*F2
         H(N) = H95(N)*F1 + H00(N)*F2
  170 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 2000 - 2005:
C
  180 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-2000)/5.D0
      F1 = 1.D0 - F2
      DO 190 N = 1,105
         G(N) = G00(N)*F1 + G05(N)*F2
         H(N) = H00(N)*F1 + H05(N)*F2
  190 CONTINUE
      GO TO 220
C
C       INTERPOLATE BETWEEN 2005 - 2010:
C
  200 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-2005)/5.D0
      F1 = 1.D0 - F2
      DO 210 N = 1,105
         G(N) = G05(N)*F1 + G10(N)*F2
         H(N) = H05(N)*F1 + H10(N)*F2
  210 CONTINUE
      GO TO 220
C
C     COEFFICIENTS FOR A GIVEN YEAR HAVE BEEN CALCULATED; NOW MULTIPLY
C     THEM BY SCHMIDT NORMALIZATION FACTORS:
C
  220 S = 1.D0
      DO 240 N = 2,14
         MN = N*(N-1)/2 + 1
         S = S*DBLE(2*N-3)/DBLE(N-1)
         G(MN) = G(MN)*S
         H(MN) = H(MN)*S
         P = S
         DO 230 M = 2,N
            AA = 1.D0
            IF (M.EQ.2) AA = 2.D0
            P = P*SQRT(AA*DBLE(N-M+1)/DBLE(N+M-2))
            MNN = MN + M - 1
            G(MNN) = G(MNN)*P
            H(MNN) = H(MNN)*P
  230    CONTINUE
  240 CONTINUE

      G_10 = -G(2)
      G_11 = G(3)
      H_11 = H(3)
C
CNOW CALCULATE GEO COMPONENTS OF THE UNIT VECTOR EzMAG, PARALLEL TO GEODIPOLE AXIS:
C     SIN(TETA0)*COS(LAMBDA0), SIN(TETA0)*SIN(LAMBDA0), AND COS(TETA0)
C         ST0 * CL0                ST0 * SL0                CT0
C
      SQ = G_11**2 + H_11**2
      SQQ = SQRT(SQ)
      SQR = SQRT(G_10**2+SQ)
      SL0 = -H_11/SQQ
      CL0 = -G_11/SQQ
      ST0 = SQQ/SQR
      CT0 = G_10/SQR
      STCL = ST0*CL0
      STSL = ST0*SL0
      CTSL = CT0*SL0
      CTCL = CT0*CL0
C
C  NOW CALCULATE GEI COMPONENTS (S1,S2,S3) OF THE UNIT VECTOR S = EX_GSE
C     POINTING FROM THE EARTH'S CENTER TO TS_SUN
C
      CALL TS_SUN_08(IY,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)
C
      S1 = COS(SRASN)*COS(SDEC)
      S2 = SIN(SRASN)*COS(SDEC)
      S3 = SIN(SDEC)
C
C    NOW CALCULATE GEI COMPONENTS (DZ1,DZ2,DZ3) OF THE UNIT VECTOR EZGSE
C     POINTING NORTHWARD AND ORTHOGONAL TO THE ECLIPTIC PLANE, AS
C (0,-SIN(OBLIQ),COS(OBLIQ)). FOR THE EPOCH 1978, OBLIQ = 23.44214 DEGS.
C     HERE WE USE A MORE ACCURATE TIME-DEPENDENT VALUE, DETERMINED AS:
C
      DJ = DBLE(365*(IY-1900)+(IY-1901)/4+IDAY) - 0.5D0 +
     *     DBLE(IHOUR*3600+MIN*60+ISEC)/86400.D0
      T = DJ/36525.D0
      OBLIQ = (23.45229D0-0.0130125D0*T)/57.2957795D0
      DZ1 = 0.D0
      DZ2 = -SIN(OBLIQ)
      DZ3 = COS(OBLIQ)
C
C   NOW WE OBTAIN GEI COMPONENTS OF THE UNIT VECTOR EYGSE=(DY1,DY2,DY3),
C  COMPLETING THE RIGHT-HANDED SYSTEM. THEY CAN BE FOUND FROM THE VECTOR
C     PRODUCT EZGSE x EXGSE = (DZ1,DZ2,DZ3) x (S1,S2,S3):
C
      DY1 = DZ2*S3 - DZ3*S2
      DY2 = DZ3*S1 - DZ1*S3
      DY3 = DZ1*S2 - DZ2*S1
C
CNOW LET'S CALCULATE GEI COMPONENTS OF THE UNIT VECTOR X = EXGSW, DIRECTED ANTIPARALLEL
CTO THE OBSERVED SOLAR WIND FLOW. FIRST, CALCULATE ITS COMPONENTS IN GSE:
C
      V = SQRT(VGSEX**2+VGSEY**2+VGSEZ**2)
      DX1 = -VGSEX/V
      DX2 = -VGSEY/V
      DX3 = -VGSEZ/V
C
C     THEN IN GEI:
C
      X1 = DX1*S1 + DX2*DY1 + DX3*DZ1
      X2 = DX1*S2 + DX2*DY2 + DX3*DZ2
      X3 = DX1*S3 + DX2*DY3 + DX3*DZ3
C
CNOW CALCULATE GEI COMPONENTS (DIP1,DIP2,DIP3) OF THE UNIT VECTOR DIP = EZ_SM = EZ_MAG,
C ALIGNED WITH THE GEODIPOLE AND POINTING NORTHWARD FROM ECLIPTIC PLANE:
C
      CGST = COS(GST)
      SGST = SIN(GST)
C
      DIP1 = STCL*CGST - STSL*SGST
      DIP2 = STCL*SGST + STSL*CGST
      DIP3 = CT0
C
CTHIS ALLOWS US TO CALCULATE GEI COMPONENTS OF THE UNIT VECTOR Y = EYGSW
CBY TAKING THE VECTOR PRODUCT DIP x X AND NORMALIZING IT TO UNIT LENGTH:
C
      Y1 = DIP2*X3 - DIP3*X2
      Y2 = DIP3*X1 - DIP1*X3
      Y3 = DIP1*X2 - DIP2*X1
      Y = SQRT(Y1*Y1+Y2*Y2+Y3*Y3)
      Y1 = Y1/Y
      Y2 = Y2/Y
      Y3 = Y3/Y
C
CAND GEI COMPONENTS OF THE UNIT VECTOR Z = EZGSW = EXGSW x EYGSW = X x Y:
C
      Z1 = X2*Y3 - X3*Y2
      Z2 = X3*Y1 - X1*Y3
      Z3 = X1*Y2 - X2*Y1
C
C     ELEMENTS OF THE MATRIX GSE TO GSW ARE THE SCALAR PRODUCTS:
C
C     E11=(EXGSE,EXGSW)  E12=(EXGSE,EYGSW)  E13=(EXGSE,EZGSW)
C     E21=(EYGSE,EXGSW)  E22=(EYGSE,EYGSW)  E23=(EYGSE,EZGSW)
C     E31=(EZGSE,EXGSW)  E32=(EZGSE,EYGSW)  E33=(EZGSE,EZGSW)
C
      E11 = S1*X1 + S2*X2 + S3*X3
      E12 = S1*Y1 + S2*Y2 + S3*Y3
      E13 = S1*Z1 + S2*Z2 + S3*Z3
      E21 = DY1*X1 + DY2*X2 + DY3*X3
      E22 = DY1*Y1 + DY2*Y2 + DY3*Y3
      E23 = DY1*Z1 + DY2*Z2 + DY3*Z3
      E31 = DZ1*X1 + DZ2*X2 + DZ3*X3
      E32 = DZ1*Y1 + DZ2*Y2 + DZ3*Y3
      E33 = DZ1*Z1 + DZ2*Z2 + DZ3*Z3
C
C     GEODIPOLE TILT ANGLE IN THE GSW SYSTEM: PSI=ARCSIN(DIP,EXGSW)
C
      SPS = DIP1*X1 + DIP2*X2 + DIP3*X3
      CPS = SQRT(1.D0-SPS**2)
      PSI = ASIN(SPS)
C
C     ELEMENTS OF THE MATRIX GEO TO GSW ARE THE SCALAR PRODUCTS:
C
C     A11=(EXGEO,EXGSW), A12=(EYGEO,EXGSW), A13=(EZGEO,EXGSW),
C     A21=(EXGEO,EYGSW), A22=(EYGEO,EYGSW), A23=(EZGEO,EYGSW),
C     A31=(EXGEO,EZGSW), A32=(EYGEO,EZGSW), A33=(EZGEO,EZGSW),
C
C     ALL THE UNIT VECTORS IN BRACKETS ARE ALREADY DEFINED IN GEI:
C
C     EXGEO=(CGST,SGST,0), EYGEO=(-SGST,CGST,0), EZGEO=(0,0,1)
C     EXGSW=(X1,X2,X3),  EYGSW=(Y1,Y2,Y3),   EZGSW=(Z1,Z2,Z3)
C                                                        AND  THEREFORE:
C
      A11 = X1*CGST + X2*SGST
      A12 = -X1*SGST + X2*CGST
      A13 = X3
      A21 = Y1*CGST + Y2*SGST
      A22 = -Y1*SGST + Y2*CGST
      A23 = Y3
      A31 = Z1*CGST + Z2*SGST
      A32 = -Z1*SGST + Z2*CGST
      A33 = Z3
C
CNOW CALCULATE ELEMENTS OF THE MATRIX MAG TO SM (ONE ROTATION ABOUT THE GEODIPOLE AXIS);
CTHEY ARE FOUND AS THE SCALAR PRODUCTS: CFI=GM22=(EYSM,EYMAG)=(EYGSW,EYMAG),
C                                     SFI=GM23=(EYSM,EXMAG)=(EYGSW,EXMAG),
C     DERIVED AS FOLLOWS:
C
CIN GEO, THE VECTORS EXMAG AND EYMAG HAVE THE COMPONENTS (CT0*CL0,CT0*SL0,-ST0)
C AND (-SL0,CL0,0), RESPECTIVELY.    HENCE, IN GEI THEIR COMPONENTS ARE:
C     EXMAG:    CT0*CL0*COS(GST)-CT0*SL0*SIN(GST)
C            CT0*CL0*SIN(GST)+CT0*SL0*COS(GST)
C            -ST0
C     EYMAG:    -SL0*COS(GST)-CL0*SIN(GST)
C            -SL0*SIN(GST)+CL0*COS(GST)
C             0
CNOW, NOTE THAT GEI COMPONENTS OF EYSM=EYGSW WERE FOUND ABOVE AS Y1, Y2, AND Y3,
C     AND WE ONLY HAVE TO COMBINE THESE QUANTITIES INTO SCALAR PRODUCTS:
C
      EXMAGX = CT0*(CL0*CGST-SL0*SGST)
      EXMAGY = CT0*(CL0*SGST+SL0*CGST)
      EXMAGZ = -ST0
      EYMAGX = -(SL0*CGST+CL0*SGST)
      EYMAGY = -(SL0*SGST-CL0*CGST)
      CFI = Y1*EYMAGX + Y2*EYMAGY
      SFI = Y1*EXMAGX + Y2*EXMAGY + Y3*EXMAGZ
C
 9000 FORMAT (/,/,1X,
     *  '****TS_RECALC WARNS: YEAR IS OUT OF INTERVAL 1965-2015: IYEAR='
     *       ,I4,/,6X,'CALCULATIONS WILL BE DONE FOR IYEAR=',I4,/)
      RETURN
      END
c
c==================================================================================

      SUBROUTINE GSWGSE(XGSW,YGSW,ZGSW,XGSE,YGSE,ZGSE,J)
C
CTHIS SUBROUTINE TRANSFORMS COMPONENTS OF ANY VECTOR BETWEEN THE STANDARD GSE
CCOORDINATE SYSTEM AND THE GEOCENTRIC SOLAR-WIND (GSW, aka GSWM), DEFINED AS FOLLOWS
C(HONES ET AL., PLANET.SPACE SCI., V.34, P.889, 1986; TSYGANENKO ET AL., JGRA,
C     V.103(A4), P.6827, 1998):
C
CIN THE GSW SYSTEM, X AXIS IS ANTIPARALLEL TO THE OBSERVED DIRECTION OF THE SOLAR WIND FLOW.
CTWO OTHER AXES, Y AND Z, ARE DEFINED IN THE SAME WAY AS FOR THE STANDARD GSM, THAT IS,
CZ AXIS ORTHOGONAL TO X AXIS, POINTS NORTHWARD, AND LIES IN THE PLANE DEFINED BY THE X-
C     AND GEODIPOLE AXIS. THE Y AXIS COMPLETES THE RIGHT-HANDED SYSTEM.
C
C    THE GSW SYSTEM BECOMES IDENTICAL TO THE STANDARD GSM IN THE CASE OF
C     A STRICTLY RADIAL SOLAR WIND FLOW.
C
C     AUTHOR:  N. A. TSYGANENKO
C     ADDED TO 2008 VERSION OF GEOPACK: JAN 27, 2008.
C
C                    J>0                       J<0
C     -----INPUT:   J,XGSW,YGSW,ZGSW          J,XGSE,YGSE,ZGSE
C     -----OUTPUT:    XGSE,YGSE,ZGSE            XGSW,YGSW,ZGSW
C
C     IMPORTANT THINGS TO REMEMBER:
C
C(1) BEFORE CALLING GSWGSE, BE SURE TO INVOKE SUBROUTINE TS_RECALC_08, IN ORDER
C       TO DEFINE ALL NECESSARY ELEMENTS OF TRANSFORMATION MATRICES
C
C(2) IN THE ABSENCE OF INFORMATION ON THE SOLAR WIND DIRECTION, E.G., WITH ONLY SCALAR
C  SPEED V KNOWN, THIS SUBROUTINE CAN BE USED TO CONVERT VECTORS TO ABERRATED
C  COORDINATE SYSTEM, TAKING INTO ACCOUNT EARTH'S ORBITAL SPEED OF 29 KM/S.
C  TO DO THAT, SPECIFY THE LAST 3 PARAMETERS IN TS_RECALC_08 AS FOLLOWS:
C       VGSEX=-V, VGSEY=29.0, VGSEZ=0.0.
C
C  IT SHOULD ALSO BE KEPT IN MIND THAT IN SOME SOLAR WIND DATABASES THE ABERRATION
C  EFFECT HAS ALREADY BEEN TAKEN INTO ACCOUNT BY SUBTRACTING 29 KM/S FROM VYGSE;
C  IN THAT CASE, THE ORIGINAL VYGSE VALUES SHOULD BE RESTORED BY ADDING BACK THE
C  29 KM/S CORRECTION. WHETHER OR NOT TO DO THAT, MUST BE VERIFIED WITH THE DATA
C  ORIGINATOR (OR CAN BE DETERMINED BY CALCULATING THE AVERAGE VGSEY OVER
C       A SUFFICIENTLY LONG TIME INTERVAL)
C
C(3) IF NO INFORMATION IS AVAILABLE ON THE SOLAR WIND SPEED, THEN SET VGSEX=-400.0
c    AND  VGSEY=VGSEZ=0. IN THAT CASE, THE GSW COORDINATE SYSTEM BECOMES
c       IDENTICAL TO THE STANDARD ONE.
C
C
C     DIRECT TRANSFORMATION:
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGSE,XGSW,YGSE,YGSW,ZGSE,ZGSW
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/AAA,E11,E21,E31,E12,E22,E32,E13,E23,E33
      DOUBLE PRECISION E11,E12,E13,E21,E22,E23,E31,E32,E33
      DOUBLE PRECISION AAA(25)
C     ..
      IF (J.GT.0) THEN
         XGSE = XGSW*E11 + YGSW*E12 + ZGSW*E13
         YGSE = XGSW*E21 + YGSW*E22 + ZGSW*E23
         ZGSE = XGSW*E31 + YGSW*E32 + ZGSW*E33
      END IF
C
C     INVERSE TRANSFORMATION: CARRIED OUT USING THE TRANSPOSED MATRIX:
C
      IF (J.LT.0) THEN
         XGSW = XGSE*E11 + YGSE*E21 + ZGSE*E31
         YGSW = XGSE*E12 + YGSE*E22 + ZGSE*E32
         ZGSW = XGSE*E13 + YGSE*E23 + ZGSE*E33
      END IF
C
      RETURN
      END
C
C========================================================================================
C
      SUBROUTINE TS_GEOMAG_08(XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,J)
C
C   CONVERTS GEOGRAPHIC (GEO) TO DIPOLE (MAG) COORDINATES OR VICE VERSA.
C
C                    J>0                       J<0
C     -----INPUT:  J,XGEO,YGEO,ZGEO           J,XMAG,YMAG,ZMAG
C     -----OUTPUT:    XMAG,YMAG,ZMAG           XGEO,YGEO,ZGEO
C
CATTENTION:  SUBROUTINE  TS_RECALC_08  MUST BE INVOKED BEFORE TS_GEOMAG_08 IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C     /B/  IF THE VALUES OF IYEAR AND/OR IDAY HAVE BEEN CHANGED
C
C     NO INFORMATION IS REQUIRED HERE ON THE SOLAR WIND VELOCITY, SO ONE
C     CAN SET VGSEX=-400.0, VGSEY=0.0, VGSEZ=0.0 IN TS_RECALC_08.
C
C     LAST MOFIFICATION:  JAN 28, 2008
C
C     AUTHOR:  N. A. TSYGANENKO
C

C     .. Scalar Arguments ..
      DOUBLE PRECISION XGEO,XMAG,YGEO,YMAG,ZGEO,ZMAG
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,AB
      DOUBLE PRECISION CL0,CT0,CTCL,CTSL,SL0,ST0,STCL,STSL
      DOUBLE PRECISION AB(26)
C     ..
      IF (J.GT.0) THEN
         XMAG = XGEO*CTCL + YGEO*CTSL - ZGEO*ST0
         YMAG = YGEO*CL0 - XGEO*SL0
         ZMAG = XGEO*STCL + YGEO*STSL + ZGEO*CT0
      ELSE
         XGEO = XMAG*CTCL - YMAG*SL0 + ZMAG*STCL
         YGEO = XMAG*CTSL + YMAG*CL0 + ZMAG*STSL
         ZGEO = ZMAG*CT0 - XMAG*ST0
      END IF

      RETURN
      END
c
c=========================================================================================
c
      SUBROUTINE GEIGEO_08(XGEI,YGEI,ZGEI,XGEO,YGEO,ZGEO,J)
C
C     CONVERTS EQUATORIAL INERTIAL (GEI) TO GEOGRAPHICAL (GEO) COORDS
C     OR VICE VERSA.
C                    J>0                J<0
C     ----INPUT:  J,XGEI,YGEI,ZGEI    J,XGEO,YGEO,ZGEO
C     ----OUTPUT:   XGEO,YGEO,ZGEO      XGEI,YGEI,ZGEI
C
CATTENTION:  SUBROUTINE  TS_RECALC_08  MUST BE INVOKED BEFORE GEIGEO_08 IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C/B/  IF THE CURRENT VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC HAVE BEEN CHANGED
C
C     NO INFORMATION IS REQUIRED HERE ON THE SOLAR WIND VELOCITY, SO ONE
C     CAN SET VGSEX=-400.0, VGSEY=0.0, VGSEZ=0.0 IN TS_RECALC_08.
C
C     LAST MODIFICATION:  JAN 28, 2008

C     AUTHOR:  N. A. TSYGANENKO
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGEI,XGEO,YGEI,YGEO,ZGEI,ZGEO
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,CGST,SGST,B
      DOUBLE PRECISION CGST,SGST
      DOUBLE PRECISION A(13),B(19)
C     ..
      IF (J.GT.0) THEN
         XGEO = XGEI*CGST + YGEI*SGST
         YGEO = YGEI*CGST - XGEI*SGST
         ZGEO = ZGEI
      ELSE
         XGEI = XGEO*CGST - YGEO*SGST
         YGEI = YGEO*CGST + XGEO*SGST
         ZGEI = ZGEO
      END IF

      RETURN
      END
C
C=======================================================================================
C
      SUBROUTINE TS_MAGSM_08(XMAG,YMAG,ZMAG,XSM,YSM,ZSM,J)
C
C CONVERTS DIPOLE (MAG) TO SOLAR MAGNETIC (SM) COORDINATES OR VICE VERSA
C
C                    J>0              J<0
C     ----INPUT: J,XMAG,YMAG,ZMAG     J,XSM,YSM,ZSM
C     ----OUTPUT:    XSM,YSM,ZSM       XMAG,YMAG,ZMAG
C
CATTENTION:  SUBROUTINE  TS_RECALC_08  MUST BE INVOKED BEFORE TS_MAGSM_08 IN THREE CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES, OR
C   /B/  IF THE VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC HAVE CHANGED, AND/OR
C/C/  IF THE VALUES OF COMPONENTS OF THE SOLAR WIND FLOW VELOCITY HAVE CHANGED
C
C     IMPORTANT NOTE:
C
C   A NON-STANDARD DEFINITION IS IMPLIED HERE FOR THE SOLAR MAGNETIC COORDINATE
C   SYSTEM:  IT IS ASSUMED THAT THE XSM AXIS LIES IN THE PLANE DEFINED BY THE
C   GEODIPOLE AXIS AND THE OBSERVED VECTOR OF THE SOLAR WIND FLOW (RATHER THAN
C   THE EARTH-TS_SUN LINE).  IN ORDER TO CONVERT MAG COORDINATES TO AND FROM THE
C   STANDARD SM COORDINATES, INVOKE TS_RECALC_08 WITH VGSEX=-400.0, VGSEY=0.0, VGSEZ=0.0
C
C     LAST MODIFICATION:  FEB 07, 2008
C
C     AUTHOR:  N. A. TSYGANENKO
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XMAG,XSM,YMAG,YSM,ZMAG,ZSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,SFI,CFI,B
      DOUBLE PRECISION CFI,SFI
      DOUBLE PRECISION A(8),B(24)
C     ..
      IF (J.GT.0) THEN
         XSM = XMAG*CFI - YMAG*SFI
         YSM = XMAG*SFI + YMAG*CFI
         ZSM = ZMAG
      ELSE
         XMAG = XSM*CFI + YSM*SFI
         YMAG = YSM*CFI - XSM*SFI
         ZMAG = ZSM
      END IF

      RETURN
      END
C
C=====================================================================================
C
      SUBROUTINE SMGSW_08(XSM,YSM,ZSM,XGSW,YGSW,ZGSW,J)
C
CCONVERTS SOLAR MAGNETIC (SM) TO GEOCENTRIC SOLAR-WIND (GSW) COORDINATES OR VICE VERSA.
C
C                  J>0                 J<0
C     -----INPUT: J,XSM,YSM,ZSM        J,XGSW,YGSW,ZGSW
C     ----OUTPUT:  XGSW,YGSW,ZGSW       XSM,YSM,ZSM
C
CATTENTION:  SUBROUTINE TS_RECALC_08 MUST BE INVOKED BEFORE SMGSW_08 IN THREE CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C     /B/  IF THE VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC HAVE BEEN CHANGED
C/C/  IF THE VALUES OF COMPONENTS OF THE SOLAR WIND FLOW VELOCITY HAVE CHANGED
C
C     IMPORTANT NOTE:
C
C   A NON-STANDARD DEFINITION IS IMPLIED HERE FOR THE SOLAR MAGNETIC (SM) COORDINATE
C   SYSTEM:  IT IS ASSUMED THAT THE XSM AXIS LIES IN THE PLANE DEFINED BY THE
C   GEODIPOLE AXIS AND THE OBSERVED VECTOR OF THE SOLAR WIND FLOW (RATHER THAN
C   THE EARTH-TS_SUN LINE).  IN ORDER TO CONVERT MAG COORDINATES TO AND FROM THE
C   STANDARD SM COORDINATES, INVOKE TS_RECALC_08 WITH VGSEX=-400.0, VGSEY=0.0, VGSEZ=0.0
C
C     LAST MODIFICATION:  FEB 07, 2008
C
C     AUTHOR:  N. A. TSYGANENKO
C

C     .. Scalar Arguments ..
      DOUBLE PRECISION XGSW,XSM,YGSW,YSM,ZGSW,ZSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,SPS,CPS,B
      DOUBLE PRECISION CPS,SPS
      DOUBLE PRECISION A(10),B(22)
C     ..
      IF (J.GT.0) THEN
         XGSW = XSM*CPS + ZSM*SPS
         YGSW = YSM
         ZGSW = ZSM*CPS - XSM*SPS
      ELSE
         XSM = XGSW*CPS - ZGSW*SPS
         YSM = YGSW
         ZSM = XGSW*SPS + ZGSW*CPS
      END IF

      RETURN
      END
C
C==========================================================================================
C
      SUBROUTINE GEOGSW(XGEO,YGEO,ZGEO,XGSW,YGSW,ZGSW,J)
C
CCONVERTS GEOGRAPHIC (GEO) TO GEOCENTRIC SOLAR-WIND (GSW) COORDINATES OR VICE VERSA.
C
C                   J>0                   J<0
C     ----- INPUT:  J,XGEO,YGEO,ZGEO    J,XGSW,YGSW,ZGSW
C     ---- OUTPUT:    XGSW,YGSW,ZGSW      XGEO,YGEO,ZGEO
C
CATTENTION:  SUBROUTINE  TS_RECALC_08  MUST BE INVOKED BEFORE GEOGSW IN THREE CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES, OR
C  /B/  IF THE VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC  HAVE CHANGED, AND/OR
C/C/  IF THE VALUES OF COMPONENTS OF THE SOLAR WIND FLOW VELOCITY HAVE CHANGED
C
CNOTE: THIS SUBROUTINE CONVERTS GEO VECTORS TO AND FROM THE SOLAR-WIND GSW COORDINATE
C   SYSTEM, TAKING INTO ACCOUNT POSSIBLE DEFLECTIONS OF THE SOLAR WIND DIRECTION FROM
C   STRICTLY RADIAL.  BEFORE CONVERTING TO/FROM STANDARD GSM COORDINATES, INVOKE TS_RECALC_08
C        WITH VGSEX=-400.0 and VGSEY=0.0, VGSEZ=0.0
C
C     LAST MODIFICATION:  FEB 07, 2008
C
C     AUTHOR:  N. A. TSYGANENKO
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGEO,XGSW,YGEO,YGSW,ZGEO,ZGSW
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/AA,A11,A21,A31,A12,A22,A32,A13,A23,A33,B
      DOUBLE PRECISION A11,A12,A13,A21,A22,A23,A31,A32,A33
      DOUBLE PRECISION AA(16),B(9)
C     ..
      IF (J.GT.0) THEN
         XGSW = A11*XGEO + A12*YGEO + A13*ZGEO
         YGSW = A21*XGEO + A22*YGEO + A23*ZGEO
         ZGSW = A31*XGEO + A32*YGEO + A33*ZGEO
      ELSE
         XGEO = A11*XGSW + A21*YGSW + A31*ZGSW
         YGEO = A12*XGSW + A22*YGSW + A32*ZGSW
         ZGEO = A13*XGSW + A23*YGSW + A33*ZGSW
      END IF

      RETURN
      END
C
C=====================================================================================
C
      SUBROUTINE GEODGEO_08(H,XMU,R,THETA,J)
C     .. Scalar Arguments ..
      DOUBLE PRECISION H,R,THETA,XMU
      INTEGER J
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ARG,BETA,COSFIMS,COSLAM,COSXMU,DEN,DPHI,PHI,PHI1,
     *                 RR,RS,R_EQ,SINLAM,SINXMU,SP,TOL,X,XMUS,Z
      INTEGER N
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ACOS,ASIN,COS,SIN,SQRT
C     ..
C     .. Data statements ..
C
CTHIS SUBROUTINE (1) CONVERTS VERTICAL LOCAL HEIGHT (ALTITUDE) H AND GEODETIC
CLATITUDE XMU INTO GEOCENTRIC COORDINATES R AND THETA (GEOCENTRIC RADIAL
CDISTANCE AND COLATITUDE, RESPECTIVELY; ALSO KNOWN AS ECEF COORDINATES),
CAS WELL AS (2) PERFORMS THE INVERSE TRANSFORMATION FROM {R,THETA} TO {H,XMU}.
C
CTHE SUBROUTINE USES WORLD GEODETIC SYSTEM WGS84 PARAMETERS FOR THE EARTH'S
CELLIPSOID. THE ANGULAR QUANTITIES (GEO COLATITUDE THETA AND GEODETIC LATITUDE
CXMU) ARE IN RADIANS, AND THE DISTANCES (GEOCENTRIC RADIUS R AND ALTITUDE H
C     ABOVE THE EARTH'S ELLIPSOID) ARE IN KILOMETERS.
C
CIF J>0, THE TRANSFORMATION IS MADE FROM GEODETIC TO GEOCENTRIC COORDINATES
C     USING SIMPLE DIRECT EQUATIONS.
CIF J<0, THE INVERSE TRANSFORMATION FROM GEOCENTRIC TO GEODETIC COORDINATES
C     IS MADE BY MEANS OF A FAST ITERATIVE ALGORITHM.
C
c-------------------------------------------------------------------------------
C                   J>0                     |            J<0
c-------------------------------------------|-----------------------------------
C--INPUT:   J        H          XMU         |    J         R          THETA
c    flag  altitude (km)  geodetic     |   flag   geocentric    spherical
c                         latitude     |         distance (km) colatitude
c                         (radians)    |                        (radians)
c-------------------------------------------|-----------------------------------
c                                           |
C----OUTPUT:         R           THETA      |          H              XMU
C            geocentric    spherical    |      altitude (km)    geodetic
C            distance (km) colatitude   |                       latitude
C                         (radians)    |                       (radians)
C-------------------------------------------------------------------------------
C
C     AUTHOR:  N. A. TSYGANENKO
c     DATE:    DEC 5, 2007
C
c
c  R_EQ is the semi-major axis of the Earth's ellipsoid, and BETA is its
c     second eccentricity squared
c
      DATA R_EQ,BETA/6378.137D0,6.73949674228D-3/
      DATA TOL/1.D-6/
C     ..
c
c     Direct transformation (GEOD=>GEO):
c
      IF (J.GT.0) THEN
         COSXMU = COS(XMU)
         SINXMU = SIN(XMU)
         DEN = SQRT(COSXMU**2+(SINXMU/(1.0D0+BETA))**2)
         COSLAM = COSXMU/DEN
         SINLAM = SINXMU/(DEN*(1.0D0+BETA))
         RS = R_EQ/SQRT(1.0D0+BETA*SINLAM**2)
         X = RS*COSLAM + H*COSXMU
         Z = RS*SINLAM + H*SINXMU
         R = SQRT(X**2+Z**2)
         THETA = ACOS(Z/R)
      END IF

c
c     Inverse transformation (GEO=>GEOD):
c
      IF (J.LT.0) THEN
         N = 0
         PHI = 1.570796327D0 - THETA
         PHI1 = PHI
   10    SP = SIN(PHI1)
C        *PT*WARNING* Constant already double-precision
         ARG = SP*(1.0D0+BETA)/SQRT(1.0D0+BETA*(2.0D0+BETA)*SP**2)
         XMUS = ASIN(ARG)
         RS = R_EQ/SQRT(1.0D0+BETA*SIN(PHI1)**2)
         COSFIMS = COS(PHI1-XMUS)
         H = SQRT((RS*COSFIMS)**2+R**2-RS**2) - RS*COSFIMS
         Z = RS*SIN(PHI1) + H*SIN(XMUS)
         X = RS*COS(PHI1) + H*COS(XMUS)
         RR = SQRT(X**2+Z**2)
         DPHI = ASIN(Z/RR) - PHI
         PHI1 = PHI1 - DPHI
         N = N + 1
         IF (ABS(DPHI).GT.TOL .AND. N.LT.100) GO TO 10
         XMU = XMUS
      END IF

      RETURN
      END
C
C=====================================================================================
C
      SUBROUTINE RHAND_08(X,Y,Z,R1,R2,R3,IOPT,PARMOD,EXNAME,INNAME)
C
CCALCULATES THE COMPONENTS OF THE RIGHT HAND SIDE VECTOR IN THE TS_GEOMAGNETIC FIELD
C    LINE EQUATION  (a subsidiary subroutine for the subroutine STEP_08)
C
C     LAST MODIFICATION:  FEB 07, 2008
C
C     AUTHOR:  N. A. TSYGANENKO
C
C
CEXNAME AND INNAME ARE NAMES OF SUBROUTINES FOR THE EXTERNAL AND INTERNAL
C     PARTS OF THE TOTAL FIELD, E.G., T96_01 AND IGRF_GSW_08
C
C     common block added by B. Rideout to signal error
      COMMON /BR_ERR/IERR
      INTEGER IERR
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION R1,R2,R3,X,Y,Z
      INTEGER IOPT
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION PARMOD(10)
C     ..
C     .. Subroutine Arguments ..
      EXTERNAL EXNAME,INNAME
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION B,BX,BXGSW,BY,BYGSW,BZ,BZGSW,HXGSW,HYGSW,HZGSW
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,DS3,BB,PSI,CC
      DOUBLE PRECISION DS3,PSI
      DOUBLE PRECISION A(12),BB(2),CC(18)
C     ..
      CALL EXNAME(IOPT,PARMOD,PSI,X,Y,Z,BXGSW,BYGSW,BZGSW)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL INNAME(X,Y,Z,HXGSW,HYGSW,HZGSW)

      BX = BXGSW + HXGSW
      BY = BYGSW + HYGSW
      BZ = BZGSW + HZGSW
      B = DS3/SQRT(BX**2+BY**2+BZ**2)
      R1 = BX*B
      R2 = BY*B
      R3 = BZ*B
      RETURN
      END
C
C===================================================================================
C
      SUBROUTINE STEP_08(X,Y,Z,DS,DSMAX,ERRIN,IOPT,PARMOD,EXNAME,INNAME)
C
CRE-CALCULATES THE INPUT VALUES {X,Y,Z} (IN GSW COORDINATES) FOR ANY POINT ON A FIELD LINE,
CBY MAKING A STEP ALONG THAT LINE USING RUNGE-KUTTA-MERSON ALGORITHM (G.N. Lance, Numerical
C      methods for high-speed computers, Iliffe & Sons, London 1960.)
CDS IS A PRESCRIBED VALUE OF THE CURRENT STEP SIZE, DSMAX IS ITS UPPER LIMIT.
CERRIN IS A PERMISSIBLE ERROR (ITS OPTIMAL VALUE SPECIFIED IN THE S/R TRACE)
CIF THE ACTUAL ERROR (ERRCUR) AT THE CURRENT STEP IS LARGER THAN ERRIN, THE STEP IS REJECTED,
C       AND THE CALCULATION IS REPEATED ANEW WITH HALVED STEPSIZE DS.
CIF ERRCUR IS SMALLER THAN ERRIN, THE STEP IS ACCEPTED, AND THE CURRENT VALUE OF DS IS RETAINE
C     D
C       FOR THE NEXT STEP.
CIF ERRCUR IS SMALLER THAN 0.04*ERRIN, THE STEP IS ACCEPTED, AND THE VALUE OF DS FOR THE NEXT
C     STEP
C       IS INCREASED BY THE FACTOR 1.5, BUT NOT LARGER THAN DSMAX.
CIOPT IS A FLAG, RESERVED FOR SPECIFYNG A VERSION OF THE EXTERNAL FIELD MODEL EXNAME.
C     ARRAY PARMOD(10) CONTAINS INPUT PARAMETERS FOR THE MODEL EXNAME.
C     EXNAME IS THE NAME OF THE SUBROUTINE FOR THE EXTERNAL FIELD MODEL.
CINNAME IS THE NAME OF THE SUBROUTINE FOR THE INTERNAL FIELD MODEL (EITHER DIP_08 OR IGRF_GSW_08
C     )
C
CALL THE ABOVE PARAMETERS ARE INPUT ONES; OUTPUT IS THE TS_RECALCULATED VALUES OF X,Y,Z
C
C     LAST MODIFICATION:  APR 21, 2008   (SEE ERRATA AS OF THIS DATE)
C
C     common block added by B. Rideout to signal error
      COMMON /BR_ERR/IERR
      INTEGER IERR
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DS,DSMAX,ERRIN,X,Y,Z
      INTEGER IOPT
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION PARMOD(10)
C     ..
C     .. Subroutine Arguments ..
      EXTERNAL EXNAME,INNAME
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ERRCUR,R11,R12,R13,R21,R22,R23,R31,R32,R33,R41,
     *                 R42,R43,R51,R52,R53
C     ..
C     .. External Subroutines ..
      EXTERNAL RHAND_08
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,SIGN
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,DS3,B
      DOUBLE PRECISION DS3
      DOUBLE PRECISION A(12),B(21)
C     ..
   10 DS3 = -DS/3.D0
      CALL RHAND_08(X,Y,Z,R11,R12,R13,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND_08(X+R11,Y+R12,Z+R13,R21,R22,R23,IOPT,PARMOD,EXNAME,
     *              INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND_08(X+.5D0*(R11+R21),Y+.5D0*(R12+R22),Z+.5D0*(R13+R23),
     *              R31,R32,R33,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND_08(X+.375D0*(R11+3.D0*R31),Y+.375D0*(R12+3.D0*R32),
     *              Z+.375D0*(R13+3.D0*R33),R41,R42,R43,IOPT,PARMOD,
     *              EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND_08(X+1.5D0*(R11-3.D0*R31+4.D0*R41),
     *              Y+1.5D0*(R12-3.D0*R32+4.D0*R42),
     *              Z+1.5D0*(R13-3.D0*R33+4.D0*R43),R51,R52,R53,IOPT,
     *              PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      ERRCUR = ABS(R11-4.5D0*R31+4.D0*R41-.5D0*R51) +
     *         ABS(R12-4.5D0*R32+4.D0*R42-.5D0*R52) +
     *         ABS(R13-4.5D0*R33+4.D0*R43-.5D0*R53)
C     AUTHOR:  N. A. TSYGANENKO
C

C
      IF (ERRCUR.GT.ERRIN) THEN
         DS = DS*.5D0
         GO TO 10
      END IF
C    READY FOR MAKING THE STEP, BUT CHECK THE ACCURACY; IF INSUFFICIENT,
C     REPEAT THE STEP WITH HALVED STEPSIZE:
C
C
      IF (ABS(DS).GT.DSMAX) THEN
         DS = SIGN(DSMAX,DS)
         GO TO 10
      END IF
C    ACCURACY IS ACCEPTABLE, BUT CHECK IF THE STEPSIZE IS NOT TOO LARGE;
C     OTHERWISE REPEAT THE STEP WITH DS=DSMAX
C
   20 X = X + .5D0*(R11+4.D0*R41+R51)
      Y = Y + .5D0*(R12+4.D0*R42+R52)
      Z = Z + .5D0*(R13+4.D0*R43+R53)
C
C     MAKING THE STEP:
C
C
      IF (ERRCUR.LT.ERRIN*.04D0 .AND. DS.LT.DSMAX/1.5D0) DS = DS*1.5D0
      RETURN
      END
CIF THE ACTUAL ERROR IS TOO SMALL (LESS THAN 4% OF ERRIN) AND DS SMALLER
C THAN DSMAX/1.5, THEN WE INCREASE THE STEPSIZE FOR THE NEXT STEP BY 50%
C
      SUBROUTINE TRACE(XI,YI,ZI,DIR,DSMAX,ERR,RLIM,R0,IOPT,PARMOD,
     *                    EXNAME,INNAME,XF,YF,ZF,XX,YY,ZZ,L,LMAX)
C
C==============================================================================
C
C
C    TRACES A FIELD LINE FROM AN ARBITRARY POINT OF SPACE TO THE EARTH'S
C     SURFACE OR TO A MODEL LIMITING BOUNDARY.
C
C     THIS SUBROUTINE ALLOWS TWO OPTIONS:
C
C(1) IF INNAME=IGRF_GSW_08, THEN THE IGRF MODEL WILL BE USED FOR CALCULATING
C   CONTRIBUTION FROM EARTH'S INTERNAL SOURCES. IN THIS CASE, SUBROUTINE
C TS_RECALC_08 MUST BE CALLED BEFORE USING TRACE, WITH PROPERLY SPECIFIED DATE,
C UNIVERSAL TIME, AND SOLAR WIND VELOCITY COMPONENTS, TO CALCULATE IN ADVANCE
C ALL QUANTITIES NEEDED FOR THE MAIN FIELD MODEL AND FOR TRANSFORMATIONS
C      BETWEEN INVOLVED COORDINATE SYSTEMS.
C
C(2) IF INNAME=DIP_08, THEN A PURE DIPOLE FIELD WILL BE USED INSTEAD OF THE IGRF MODEL.
C IN THIS CASE, THE SUBROUTINE TS_RECALC_08 MUST ALSO BE CALLED BEFORE TRACE.
C      HERE ONE CAN CHOOSE EITHER TO
C (a) CALCULATE DIPOLE TILT ANGLE BASED ON DATE, TIME, AND SOLAR WIND DIRECTION,
COR (b) EXPLICITLY SPECIFY THAT ANGLE, WITHOUT ANY REFERENCE TO DATE/UT/SOLAR WIND.
C IN THE LAST CASE (b), THE SINE (SPS) AND COSINE (CPS) OF THE DIPOLE TILT
C ANGLE MUST BE SPECIFIED IN ADVANCE (BUT AFTER HAVING CALLED TS_RECALC_08) AND FORWARDED
C IN THE COMMON BLOCK /GEOPACK1/ (IN ITS 11th AND 12th ELEMENTS, RESPECTIVELY).
C IN THIS CASE THE ROLE OF THE SUBROUTINE TS_RECALC_08 IS REDUCED TO ONLY CALCULATING
C      THE COMPONENTS OF THE EARTH'S DIPOLE MOMENT.
C
C     ------------- INPUT PARAMETERS:
C
CXI,YI,ZI - GSW COORDS OF THE FIELD LINE STARTING POINT (IN EARTH RADII, 1 RE = 6371.2 km),
C
CDIR - SIGN OF THE TRACING DIRECTION: IF DIR=1.0 THEN THE TRACING IS MADE ANTIPARALLEL
CTO THE TOTAL FIELD VECTOR (E.G., FROM NORTHERN TO SOUTHERN CONJUGATE POINT);
CIF DIR=-1.0 THEN THE TRACING PROCEEDS IN THE OPPOSITE DIRECTION, THAT IS, PARALLEL TO
C     THE TOTAL FIELD VECTOR.
C
CDSMAX - UPPER LIMIT ON THE STEPSIZE (SETS A DESIRED MAXIMAL SPACING BETWEEN
C                 THE FIELD LINE POINTS)
C
CERR - PERMISSIBLE STEP ERROR. A REASONABLE ESTIMATE PROVIDING A SUFFICIENT ACCURACY FOR MOST
C    APPLICATIONS IS ERR=0.0001. SMALLER/LARGER VALUES WILL RESULT IN LARGER/SMALLER NUMBER
C    OF STEPS AND, HENCE, OF OUTPUT FIELD LINE POINTS. NOTE THAT USING MUCH SMALLER VALUES
C    OF ERR MAY REQUIRE USING A DOUBLE PRECISION VERSION OF THE ENTIRE PACKAGE.
C
CR0 -  RADIUS OF A SPHERE (IN RE), DEFINING THE INNER BOUNDARY OF THE TRACING REGION
C         (USUALLY, EARTH'S SURFACE OR THE IONOSPHERE, WHERE R0~1.0)
C    IF THE FIELD LINE REACHES THAT SPHERE FROM OUTSIDE, ITS INBOUND TRACING IS
C    TERMINATED AND THE CROSSING POINT COORDINATES XF,YF,ZF  ARE CALCULATED.
C
CRLIM - RADIUS OF A SPHERE (IN RE), DEFINING THE OUTER BOUNDARY OF THE TRACING REGION;
C    IF THE FIELD LINE REACHES THAT BOUNDARY FROM INSIDE, ITS OUTBOUND TRACING IS
C    TERMINATED AND THE CROSSING POINT COORDINATES XF,YF,ZF ARE CALCULATED.
C
CIOPT - A MODEL INDEX; CAN BE USED FOR SPECIFYING A VERSION OF THE EXTERNAL FIELD
C  MODEL (E.G., A NUMBER OF THE KP-INDEX INTERVAL). ALTERNATIVELY, ONE CAN USE THE ARRAY
C  PARMOD FOR THAT PURPOSE (SEE BELOW); IN THAT CASE IOPT IS JUST A DUMMY PARAMETER.
C
CPARMOD -  A 10-ELEMENT ARRAY CONTAINING INPUT PARAMETERS NEEDED FOR A UNIQUE
C SPECIFICATION OF THE EXTERNAL FIELD MODEL. THE CONCRETE MEANING OF THE COMPONENTS
C      OF PARMOD DEPENDS ON A SPECIFIC VERSION OF THAT MODEL.
C
CEXNAME - NAME OF A SUBROUTINE PROVIDING COMPONENTS OF THE EXTERNAL MAGNETIC FIELD
C     (E.G., T89, OR T96_01, ETC.).
CINNAME - NAME OF A SUBROUTINE PROVIDING COMPONENTS OF THE INTERNAL MAGNETIC FIELD
C     (EITHER DIP_08 OR IGRF_GSW_08).
C
CLMAX - MAXIMAL LENGTH OF THE ARRAYS XX,YY,ZZ, IN WHICH COORDINATES OF THE FIELD
C     LINE POINTS ARE STORED. LMAX SHOULD BE SET EQUAL TO THE ACTUAL LENGTH OF
C     THE ARRAYS, DEFINED IN THE MAIN PROGRAM AS ACTUAL ARGUMENTS OF THIS SUBROUTINE.
C
C     -------------- OUTPUT PARAMETERS:
C
C   XF,YF,ZF - GSW COORDINATES OF THE ENDPOINT OF THE TRACED FIELD LINE.
CXX,YY,ZZ - ARRAYS OF LENGTH LMAX, CONTAINING COORDINATES OF THE FIELD LINE POINTS.
C  L - ACTUAL NUMBER OF FIELD LINE POINTS, GENERATED BY THIS SUBROUTINE.
C
C     ----------------------------------------------------------
C
C     LAST MODIFICATION:  JAN 30, 2008.
C
C     common block added by B. Rideout to signal error
      COMMON /BR_ERR/IERR
      INTEGER IERR
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DIR,DSMAX,ERR,R0,RLIM,XF,XI,YF,YI,ZF,ZI
      INTEGER IOPT,L,LMAX
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION PARMOD(10),XX(LMAX),YY(LMAX),ZZ(LMAX)
C     ..
C     .. Subroutine Arguments ..
      EXTERNAL EXNAME,INNAME
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AD,AL,DR,DRP,DS,FC,R,R1,R2,R3,RR,RYZ,X,XR,Y,YR,Z,
     *                 ZR
      INTEGER NREV
C     ..
C     .. External Subroutines ..
      EXTERNAL RHAND_08,STEP_08
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/AA,DD,BB
      DOUBLE PRECISION DD
      DOUBLE PRECISION AA(12),BB(21)
C     ..
      L = 0
      NREV = 0
      ZR = 0.0D0
      YR = 0.0D0
      XR = 0.0D0
      DD = DIR
C     AUTHOR:  N. A. TSYGANENKO
C
C
      DS = 0.5D0*DIR
      X = XI
      Y = YI
      Z = ZI
C     initialize error flag
      IERR = 0
C
C     INITIALIZE THE STEP SIZE AND STARTING PONT:
C
c
chere we call RHAND_08 just to find out the sign of the radial component of the field
      CALL RHAND_08(X,Y,Z,R1,R2,R3,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          L = 999
          RETURN
      END IF
      AD = 0.01D0
      IF (X*R1+Y*R2+Z*R3.LT.0.D0) AD = -0.01D0
cvector, and to determine the initial direction of the tracing (i.e., either away
c     or towards Earth):
c
C
c     |AD|=0.01 and its sign follows the rule:
c(1) if DIR=1 (tracing antiparallel to B vector) then the sign of AD is the same as of Br
      RR = SQRT(X**2+Y**2+Z**2) + AD
c(2) if DIR=-1 (tracing parallel to B vector) then the sign of AD is opposite to that of Br
   10 L = L + 1 
      IF (L.GT.LMAX) GO TO 40
      XX(L) = X
      YY(L) = Y
      ZZ(L) = Z
      RYZ = Y**2 + Z**2
      R2 = X**2 + RYZ
      R = SQRT(R2)
cAD is defined in order to initialize the value of RR (radial distance at previous step):

c
C
ccheck if the line hit the outer tracing boundary; if yes, then terminate
cthe tracing (label 8). The outer boundary is assumed reached, when the line
      IF (R.GT.RLIM .OR. RYZ.GT.1600.D0 .OR. X.GT.20.D0) GO TO 50
ccrosses any of the 3 surfaces: (1) a sphere R=RLIM, (2) a cylinder of radius 40Re,
c     coaxial with the XGSW axis, (3) the plane X=20Re:

c
      IF (R.LT.R0 .AND. RR.GT.R) GO TO 30
ccheck whether or not the inner tracing boundary was crossed from outside,
cif yes, then calculate the footpoint position by interpolation (go to label 6):
c
      IF (R.GE.RR .OR. R.GE.3.D0) GO TO 20

ccheck if we are moving outward, or R is still larger than 3Re; if yes, proceed further:
c
c
cnow we entered inside the sphere R=3: to avoid too large steps (and hence
      FC = 0.2D0
      IF (R-R0.LT.0.05D0) FC = 0.05D0
      AL = FC*(R-R0+0.2D0)
      DS = DIR*AL
cinaccurate interpolated position of the footpoint), enforce the progressively
   20 XR = X
      YR = Y
      ZR = Z
c     smaller stepsize values as we approach the inner boundary R=R0:
      DRP = R - RR
      RR = R
c
      CALL STEP_08(X,Y,Z,DS,DSMAX,ERR,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          L = 999
          RETURN
      END IF
c
c
c
c
      R = SQRT(X**2+Y**2+Z**2)
      DR = R - RR
      IF (DRP*DR.LT.0.D0) NREV = NREV + 1
      IF (NREV.GT.2) GO TO 50
Ccheck the total number NREV of changes in the tracing radial direction; (NREV.GT.2) means
      GO TO 10
cthat the line started making multiple loops, in which case we stop the process:
C
C
c
   30 R1 = (R0-R)/(RR-R)
      X = X - (X-XR)*R1
      Y = Y - (Y-YR)*R1
      Z = Z - (Z-ZR)*R1
      GO TO 50
C   40 WRITE (*,FMT=9000)
       L = LMAX
   40  L = LMAX
   50 XF = X
      YF = Y
      ZF = Z
cfind the footpoint position by interpolating between the current and previous
c     field line points:
c
C
Creplace the coordinates of the last (L-th) point in the XX,YY,ZZ arrays
      XX(L) = XF
      YY(L) = YF
      ZZ(L) = ZF
Cso that they correspond to the estimated footpoint position {XF,YF,ZF},
      RETURN
 9000 FORMAT (/,/,1X,'**** COMPUTATIONS IN THE SUBROUTINE TRACE ARE',
     *       ' TERMINATED: THE NUMBER OF POINTS EXCEEDED LMAX ****',/,/)
      END
c     satisfying:  sqrt(XF**2+YF**2+ZF**2}=R0
C
C
      SUBROUTINE SHUETAL_MGNP_08(XN_PD,VEL,BZIMF,XGSW,YGSW,ZGSW,XMGNP,
     *                           YMGNP,ZMGNP,DIST,ID)
c
C====================================================================================
C
C
CFOR ANY POINT OF SPACE WITH COORDINATES (XGSW,YGSW,ZGSW) AND SPECIFIED CONDITIONS
C     IN THE INCOMING SOLAR WIND, THIS SUBROUTINE:
C
C(1) DETERMINES IF THE POINT (XGSW,YGSW,ZGSW) LIES INSIDE OR OUTSIDE THE
C      MODEL MAGNETOPAUSE OF SHUE ET AL. (JGR-A, V.103, P. 17691, 1998).
C
C(2) CALCULATES THE GSW POSITION OF A POINT {XMGNP,YMGNP,ZMGNP}, LYING AT THE MODEL
C MAGNETOPAUSE AND ASYMPTOTICALLY TENDING TO THE NEAREST BOUNDARY POINT WITH
C RESPECT TO THE OBSERVATION POINT {XGSW,YGSW,ZGSW}, AS IT APPROACHES THE MAGNETO-
C      PAUSE.
C
CINPUT: XN_PD - EITHER SOLAR WIND PROTON NUMBER DENSITY (PER C.C.) (IF VEL>0)
C               OR THE SOLAR WIND RAM PRESSURE IN NANOPASCALS   (IF VEL<0)
C         BZIMF - IMF BZ IN NANOTESLAS
C
C         VEL - EITHER SOLAR WIND VELOCITY (KM/SEC)
C              OR ANY NEGATIVE NUMBER, WHICH INDICATES THAT XN_PD STANDS
C                FOR THE SOLAR WIND PRESSURE, RATHER THAN FOR THE DENSITY
C
C    XGSW,YGSW,ZGSW - GSW POSITION OF THE OBSERVATION POINT IN EARTH RADII
C
C     OUTPUT: XMGNP,YMGNP,ZMGNP - GSW POSITION OF THE BOUNDARY POINT
C     DIST - DISTANCE (IN RE) BETWEEN THE OBSERVATION POINT (XGSW,YGSW,ZGSW)
C                 AND THE MODEL NAGNETOPAUSE
C      ID -  POSITION FLAG:  ID=+1 (-1) MEANS THAT THE OBSERVATION POINT
C         LIES INSIDE (OUTSIDE) OF THE MODEL MAGNETOPAUSE, RESPECTIVELY.
C
C     OTHER SUBROUTINES USED: T96_MGNP_08
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BZIMF,DIST,VEL,XGSW,XMGNP,XN_PD,YGSW,YMGNP,ZGSW,
     *                 ZMGNP
      INTEGER ID
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ALPHA,CT,DR,DS,DT,F,GRADF,GRADF_R,GRADF_T,P,PHI,
     *                 R,R0,RHO,RHO2,RM,ST,T,XMT96,YMT96,ZMT96
      INTEGER ID96,NIT
C     ..
C     .. External Subroutines ..
      EXTERNAL T96_MGNP_08
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,DLOG,SIN,SQRT,TANH
C     ..
      IF (VEL.LT.0.D0) THEN
         P = XN_PD
      ELSE
         P = 1.94D-6*XN_PD*VEL**2
      END IF
c          AUTHOR:  N.A. TSYGANENKO,
C          DATE:    APRIL 4, 2003.
C

c
cDEFINE THE ANGLE PHI, MEASURED DUSKWARD FROM THE NOON-MIDNIGHT MERIDIAN PLANE;
      IF (YGSW.NE.0.D0 .OR. ZGSW.NE.0.D0) THEN
         PHI = ATAN2(YGSW,ZGSW)
      ELSE
         PHI = 0.D0
      END IF
CIF THE OBSERVATION POINT LIES ON THE X AXIS, THE ANGLE PHI CANNOT BE UNIQUELY
C     DEFINED, AND WE SET IT AT ZERO:
c
C
      ID = -1
      R0 = (10.22D0+1.29D0*TANH(0.184D0*(BZIMF+8.14D0)))*
     *     P**(-.15151515D0)
      ALPHA = (0.58D0-0.007D0*BZIMF)*(1.D0+0.024D0*DLOG(P))
      R = SQRT(XGSW**2+YGSW**2+ZGSW**2)
      RM = R0*(2.D0/(1.D0+XGSW/R))**ALPHA
      IF (R.LE.RM) ID = +1
CFIRST, FIND OUT IF THE OBSERVATION POINT LIES INSIDE THE SHUE ET AL BDRY
C     AND SET THE VALUE OF THE ID FLAG:
C
C
C   NOW, FIND THE CORRESPONDING T96 MAGNETOPAUSE POSITION, TO BE USED AS
      CALL T96_MGNP_08(P,-1.D0,XGSW,YGSW,ZGSW,XMT96,YMT96,ZMT96,DIST,
     *                 ID96)
C  A STARTING APPROXIMATION IN THE SEARCH OF A CORRESPONDING SHUE ET AL.
      RHO2 = YMT96**2 + ZMT96**2
      R = SQRT(RHO2+XMT96**2)
      ST = SQRT(RHO2)/R
      CT = XMT96/R
C     BOUNDARY POINT:
C
C
C
      NIT = 0
C    NOW, USE NEWTON'S ITERATIVE METHOD TO FIND THE NEAREST POINT AT THE
   10 T = ATAN2(ST,CT)
      RM = R0*(2.D0/(1.D0+CT))**ALPHA
C     SHUE ET AL.'S BOUNDARY:
      F = R - RM
      GRADF_R = 1.D0
      GRADF_T = -ALPHA/R*RM*ST/(1.D0+CT)
      GRADF = SQRT(GRADF_R**2+GRADF_T**2)
C
      DR = -F/GRADF**2
      DT = DR/R*GRADF_T

      R = R + DR
      T = T + DT
      ST = SIN(T)
      CT = COS(T)

      DS = SQRT(DR**2+(R*DT)**2)

      NIT = NIT + 1

C      IF (NIT.GT.1000) THEN
C         PRINT *,
C     *  ' BOUNDARY POINT COULD NOT BE FOUND; ITERATIONS DO NOT CONVERGE'
C      END IF

      IF (DS.GT.1.D-4) GO TO 10

      XMGNP = R*COS(T)
      RHO = R*SIN(T)

      YMGNP = RHO*SIN(PHI)
      ZMGNP = RHO*COS(PHI)

      DIST = SQRT((XGSW-XMGNP)**2+(YGSW-YMGNP)**2+(ZGSW-ZMGNP)**2)

      RETURN
      END



      SUBROUTINE T96_MGNP_08(XN_PD,VEL,XGSW,YGSW,ZGSW,XMGNP,YMGNP,ZMGNP,
     *                       DIST,ID)
C
C=======================================================================================
C
C
CFOR ANY POINT OF SPACE WITH GIVEN COORDINATES (XGSW,YGSW,ZGSW), THIS SUBROUTINE DEFINES
CTHE POSITION OF A POINT (XMGNP,YMGNP,ZMGNP) AT THE T96 MODEL MAGNETOPAUSE WITH THE
CSAME VALUE OF THE ELLIPSOIDAL TAU-COORDINATE, AND THE DISTANCE BETWEEN THEM.  THIS IS
CNOT THE SHORTEST DISTANCE D_MIN TO THE BOUNDARY, BUT DIST ASYMPTOTICALLY TENDS TO D_MIN,
C     AS THE OBSERVATION POINT GETS CLOSER TO THE MAGNETOPAUSE.
C
CINPUT: XN_PD - EITHER SOLAR WIND PROTON NUMBER DENSITY (PER C.C.) (IF VEL>0)
C               OR THE SOLAR WIND RAM PRESSURE IN NANOPASCALS   (IF VEL<0)
C         VEL - EITHER SOLAR WIND VELOCITY (KM/SEC)
C              OR ANY NEGATIVE NUMBER, WHICH INDICATES THAT XN_PD STANDS
C                FOR THE SOLAR WIND PRESSURE, RATHER THAN FOR THE DENSITY
C
C    XGSW,YGSW,ZGSW - COORDINATES OF THE OBSERVATION POINT IN EARTH RADII
C
COUTPUT: XMGNP,YMGNP,ZMGNP - GSW POSITION OF THE BOUNDARY POINT, HAVING THE SAME
C      VALUE OF TAU-COORDINATE AS THE OBSERVATION POINT (XGSW,YGSW,ZGSW)
C          DIST -  THE DISTANCE BETWEEN THE TWO POINTS, IN RE,
C     ID -    POSITION FLAG; ID=+1 (-1) MEANS THAT THE POINT (XGSW,YGSW,ZGSW)
C          LIES INSIDE (OUTSIDE) THE MODEL MAGNETOPAUSE, RESPECTIVELY.
C
C   THE PRESSURE-DEPENDENT MAGNETOPAUSE IS THAT USED IN THE T96_01 MODEL
C   (TSYGANENKO, JGR, V.100, P.5599, 1995; ESA SP-389, P.181, OCT. 1996)
C
c     AUTHOR:  N.A. TSYGANENKO
C     DATE:    AUG.1, 1995, REVISED APRIL 3, 2003.
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DIST,VEL,XGSW,XMGNP,XN_PD,YGSW,YMGNP,ZGSW,ZMGNP
      INTEGER ID
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,A0,ARG,PD,PHI,RAT,RAT16,RHO,RHOMGNP,S0,S00,
     *                 SIGMA,SQ1,SQ2,TAU,X0,X00,XDZT,XKSI,XM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,SIN,SQRT
C     ..
      IF (VEL.LT.0.D0) THEN
         PD = XN_PD
      ELSE
         PD = 1.94D-6*XN_PD*VEL**2
CDEFINE SOLAR WIND DYNAMIC PRESSURE (NANOPASCALS, ASSUMING 4% OF ALPHA-PARTICLES),
      END IF
C     IF NOT EXPLICITLY SPECIFIED IN THE INPUT:

C
      RAT = PD/2.0D0
      RAT16 = RAT**0.14D0
C
C     RATIO OF PD TO THE AVERAGE PRESSURE, ASSUMED EQUAL TO 2 nPa:


C(THE POWER INDEX 0.14 IN THE SCALING FACTOR IS THE BEST-FIT VALUE OBTAINED FROM DATA
C     AND USED IN THE T96_01 VERSION)
      A0 = 70.D0
      S00 = 1.08D0
      X00 = 5.48D0
C
C     VALUES OF THE MAGNETOPAUSE PARAMETERS FOR  PD = 2 nPa:
C
      A = A0/RAT16
      S0 = S00
      X0 = X00/RAT16
      XM = X0 - A
C
C  VALUES OF THE MAGNETOPAUSE PARAMETERS, SCALED BY THE ACTUAL PRESSURE:
C
C
C(XM IS THE X-COORDINATE OF THE "SEAM" BETWEEN THE ELLIPSOID AND THE CYLINDER)
C
C     (FOR DETAILS OF THE ELLIPSOIDAL COORDINATES, SEE THE PAPER:
      IF (YGSW.NE.0.D0 .OR. ZGSW.NE.0.D0) THEN
         PHI = ATAN2(YGSW,ZGSW)
      ELSE
         PHI = 0.D0
      END IF
C      N.A.TSYGANENKO, SOLUTION OF CHAPMAN-FERRARO PROBLEM FOR AN
      RHO = SQRT(YGSW**2+ZGSW**2)
C      ELLIPSOIDAL MAGNETOPAUSE, PLANET.SPACE SCI., V.37, P.1037, 1989).
      IF (XGSW.LT.XM) THEN
         XMGNP = XGSW
         RHOMGNP = A*SQRT(S0**2-1)
         YMGNP = RHOMGNP*SIN(PHI)
         ZMGNP = RHOMGNP*COS(PHI)
         DIST = SQRT((XGSW-XMGNP)**2+(YGSW-YMGNP)**2+(ZGSW-ZMGNP)**2)
         IF (RHOMGNP.GT.RHO) ID = +1
         IF (RHOMGNP.LE.RHO) ID = -1
         RETURN
      END IF
C
      XKSI = (XGSW-X0)/A + 1.D0
      XDZT = RHO/A
      SQ1 = SQRT((1.D0+XKSI)**2+XDZT**2)
      SQ2 = SQRT((1.D0-XKSI)**2+XDZT**2)
      SIGMA = 0.5D0*(SQ1+SQ2)
      TAU = 0.5D0*(SQ1-SQ2)
C
C
C
      XMGNP = X0 - A*(1.D0-S0*TAU)
      ARG = (S0**2-1.D0)*(1.D0-TAU**2)
      IF (ARG.LT.0.D0) ARG = 0.D0
      RHOMGNP = A*SQRT(ARG)
      YMGNP = RHOMGNP*SIN(PHI)
      ZMGNP = RHOMGNP*COS(PHI)
C
C     NOW CALCULATE (X,Y,Z) FOR THE CLOSEST POINT AT THE MAGNETOPAUSE
C
C
CNOW CALCULATE THE DISTANCE BETWEEN THE POINTS {XGSW,YGSW,ZGSW} AND {XMGNP,YMGNP,ZMGNP}:
      DIST = SQRT((XGSW-XMGNP)**2+(YGSW-YMGNP)**2+(ZGSW-ZMGNP)**2)
C(IN GENERAL, THIS IS NOT THE SHORTEST DISTANCE D_MIN, BUT DIST ASYMPTOTICALLY TENDS
      IF (SIGMA.GT.S0) ID = -1
      IF (SIGMA.LE.S0) ID = +1
C     TO D_MIN, AS WE ARE GETTING CLOSER TO THE MAGNETOPAUSE):
      RETURN
      END

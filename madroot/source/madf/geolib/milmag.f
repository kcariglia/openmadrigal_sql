C     $Id: milmag.f 7602 2023-09-15 22:09:41Z brideout $
C
      SUBROUTINE MILMAG(TM,RKM,ST,CT,SPH,CPH,BR,BT,BP,B)
C
C     MILMAG evaluates the geomagnetic field at a point specified by
C     its geocentric coordinates.
C
C     Modified by B. Rideout - Sep 9, 2020
C     This method is now simply a thin wrapper around igrf13.f,
C     method igrf13syn.  See igrf13.f for details
C
C     Modified by B. Rideout - Aug. 31, 2005
C     This method is now simply a thin wrapper around Geopack-2005 code,
C     method igrf_geo.  See Geopack-2005.f for details
C
C     Modified by B. Rideout - Dec. 26, 2002
C     This method is now simply a thin wrapper around geo-cgm code,
C     method igrf.  See geo-cgm.f for details


C
C       Input:
C              TM - time in years for desired field (e.g. 1971.25)
C             RKM - geocentric distance (km)
C           ST,CT - sin and cos of geocentric colatitude
C         SPH,CPH - sin and cos of east longitude
C
C      Output:
C        BR,BT,BP - geocentric field components (gauss)
C               B - magnitude of field (gauss)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION B,BP,BR,BT,CPH,CT,RKM,SPH,ST,TM
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION GCLAT,GLON
      DOUBLE PRECISION R,T,F,RBR,RBT,RBP,RB
      INTEGER IYEAR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ASIN,ANINT,SQRT
C      .. external functions
      EXTERNAL igrf13syn
      
      BR = 0.0D0
      BT = 0.0D0
      BP = 0.0D0
      RBR = 0.0D0
      RBT = 0.0D0
      RBP = 0.0D0
      RB = 0.0D0

C     convert from sin/cos to GCLAT, GLON
      IF (ST .GE. 0.0 .AND. CT .GE. 0.0) THEN
          GCLAT = ASIN(ST)*57.2958
      ELSE IF (ST .GE. 0.0 .AND. CT .LE. 0.0) THEN
          GCLAT = 180 - ASIN(ST)*57.2958
      ELSE IF (ST .LE. 0.0 .AND. CT .LE. 0.0) THEN
          GCLAT = 180 - ASIN(ST)*57.2958
      ELSE
          GCLAT = ASIN(ST)*57.2958 + 360.0
      END IF

C     leave GCLAT as degrees

      IF (SPH .GE. 0.0 .AND. CPH .GE. 0.0) THEN
          GLON = ASIN(SPH)*57.2958
      ELSE IF (SPH .GE. 0.0 .AND. CPH .LE. 0.0) THEN
          GLON = 180 - ASIN(SPH)*57.2958
      ELSE IF (SPH .LE. 0.0 .AND. CPH .LE. 0.0) THEN
          GLON = 180 - ASIN(SPH)*57.2958
      ELSE
          GLON = ASIN(SPH)*57.2958 + 360.0
      END IF

C     leave GLON as degrees

      CALL igrf13syn(0,TM,2,RKM,GCLAT,GLON,RBT,RBP,RBR,RB)

C     Set outputs as double percision
      BR=DBLE(RBR)*(-1E-5)
      BT=DBLE(RBT)*(-1E-5)
      BP=DBLE(RBP)*1E-5
      B  = SQRT(BT*BT + BP*BP + BR*BR)

      RETURN
C
      END

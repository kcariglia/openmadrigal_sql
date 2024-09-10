C     A simple test prgram that calls IRI_SUB

C     $Id: testIRI.f 6723 2019-01-30 16:12:18Z brideout $

C     Declare variables
      INTEGER JF(30),JMAG,IYYYY,MMDD,IAP3(13)
      REAL ALATI,ALONG,DHOUR,HEIBEG,HEIEND,HEISTP
      REAL F107,OUTF(20,100),OARR(50)
      REAL start,finish

      call initialize

C     Set hard-coded values
C      DATA JF/1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,
C     * 1,1,1,1,1,0,1,0,1,1,1,1,0,0,0/
      DATA JF/1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,
     * 1,1,1,1,1,0,1,1,1,1,1,1,0,0,0/
      DATA IAP3/5,7,27,32,67,32,22,22,27,22,39,56,56/
      JMAG=0
      IYYYY=2000
      MMDD=101
      ALATI=42.599998
      ALONG=288.50000
      DHOUR=25.0
      HEIBEG=270.0
      HEIEND=271.0
      HEISTP=100.0
      F107=1355.9999
      

      call IRI_SUB(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,
     *           DHOUR,HEIBEG,HEIEND,HEISTP,OUTF,OARR,
     *	         IAP3,f107)

      print *,'First call: ', OUTF(1,1),OUTF(2,1),OUTF(3,1)
      print *,OUTF(4,1),OUTF(5,1),OUTF(6,1),OUTF(7,1)
      print *,OUTF(8,1),OUTF(9,1),OUTF(10,1),OUTF(11,1)

      call cpu_time(start)

      call IRI_SUB(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,
     *           DHOUR,HEIBEG,HEIEND,HEISTP,OUTF,OARR,
     *	         IAP3,f107)

      print *,'Second call: ', OUTF(1,1),OUTF(2,1),OUTF(3,1)
      print *,OUTF(4,1),OUTF(5,1),OUTF(6,1),OUTF(7,1)
      print *,OUTF(8,1),OUTF(9,1),OUTF(10,1),OUTF(11,1)

      call cpu_time(finish)
      print '("Time = ",f10.5," seconds.")',finish-start
      END

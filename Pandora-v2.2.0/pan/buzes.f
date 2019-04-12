      subroutine BUZES
     $(NO,NVY,NRAD,K,RADI,DPTH,WTAB,XINT,FINT)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Prints scaled intensity values, for TOTILA.
C     !DASH
      save
C     !DASH
      real*8 DPTH, FINT, RADI, WTAB, XINT, XMAX, XMIN
      integer I, K, KE, KOUNT, KS, NO, NRAD, NVY
      character LINE*101
C     !COM
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
      external  HILARA, DARAS, PISIDIA, LINER, PRIVET, ZEBUS, HI, BYE
      intrinsic min, max
C
C               DPTH(NRAD), RADI(NRAD), XINT(NRAD,KM), FINT(NRAD,KM),
      dimension DPTH(*),    RADI(*),    XINT(*),       FINT(*),
C
C               WTAB(KM)
     $          WTAB(*)
C     !EJECT
C
      call HI ('BUZES')
C     !BEG
C---- Get scaled data
      call HILARA      (XINT,NRAD,K,FINT,XMIN,XMAX,KOUNT)
      if(KOUNT.gt.1) then
C----   Set up subset to be printed
        KS = max((ICORE-16),1)
        KE = min((KS+32),K)
C
C----   Print heading
        call DARAS     (NO,NVY,XMAX,XMIN,KS,KE,LINE)
C
C----   Compose and print data lines
        do 101 I = 1,NRAD
          call PISIDIA (FINT,NRAD,K,I,KS,KE,LINE)
          write (NO,100) DPTH(I),RADI(I),LINE
  100     format(' ',1PE12.5,E11.4,A101)
  101   continue
C
C----   Print wavelengths/wavenumbers
        call ZEBUS     (NO,WTAB,K)
      end if
C     !END
      call BYE ('BUZES')
C
      return
      end

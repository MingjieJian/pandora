      subroutine VICI
     $(NO,WTAB,K,SF,DF,TF,R1N,TOT)
C
C     Rudolf Loeser, 1981 Aug 26
C---- Prints Flux profiles, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 CMPKM, DF, F, R1N, RD, RD2, SF, TF, WTAB
      integer I, K, NF, NO
      character TOT*(*), W*12
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
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C     !DASH
      external DIVIDE, ENCODED, LINER, SHIM, HI, BYE
C
C               WTAB(KM), SF(KM), DF(KM), TF(KM)
      dimension WTAB(*),  SF(*),  DF(*),  TF(*)
C     !EJECT
C
      call HI ('VICI')
C     !BEG
      if(NO.gt.0) then
        RD  = R1N*CMPKM
        RD2 = RD**2
        write (NO,100) TOT,RD,'WN'
  100   format(' ','---------------------------------',10X,A/
     $         ' ','Flux Profiles, in ergs/cm**2/s/Hz'//
     $         ' ','Disk Radius RD (cm) =',1PE18.3//
     $         ' ',8X,A2,12X,'Disk',6X,'Shell',6X,'Total',3X,
     $             'Total/RD**2')
        call LINER     (1,NO)
C
        do 102 I = 1,K
          call DIVIDE  (TF(I),RD2,F)
          call ENCODED (WTAB(I),W,12,12,1,NF)
          write (NO,101) I,W,DF(I),SF(I),TF(I),F
  101     format(' ',I3,1X,A12,1X,1P4E11.3)
          call SHIM    (I,5,NO)
  102   continue
      end if
C     !END
      call BYE ('VICI')
C
      return
      end

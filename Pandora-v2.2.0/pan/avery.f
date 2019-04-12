      subroutine AVERY
     $(NWV,ISTAR,WAVES,WAVMN,WAVMX)
C
C     Rudolf Loeser, 1992 Aug 03
C---- Adds additional detail to WAVES, for AUBREY.
C     !DASH
      save
C     !DASH
      real*8 D, FACT, TEN, W, WAVE, WAVES, WAVMN, WAVMX, WHI, WL, WLO,
     $       X
      integer I, ISTAR, K, NWV
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C     !DASH
      external  HI, BYE
      intrinsic aint
C
C               WAVES(NWV)
      dimension WAVES(*)
C
      call HI ('AVERY')
C     !BEG
      K = NWV-ISTAR
      if(K.gt.0) then
        WLO = log10(WAVMN)
        WHI = log10(WAVMX)
        D = K+1
        X = (WHI-WLO)/D
C
        W = WLO
        do 100 I = (ISTAR+1),NWV
          W    = W+X
          WAVE = TEN**W
C
          WL   = log10(WAVE)
          K    = WL
          FACT = TEN**(8-K)
          WAVE = aint(WAVE*FACT)
C
          WAVES(I) = WAVE/FACT
  100   continue
      end if
C     !END
      call BYE ('AVERY')
C
      return
      end

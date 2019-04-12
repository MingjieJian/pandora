      subroutine FIMOOR
     $(W,K,DL,XHZ,EW)
C
C     Rudolf Loeser, 2000 May 17
C---- Supervises the calculation of equivalent width (EW).
C     !DASH
      save
C     !DASH
      real*8 DL, EW, W, XHZ
      integer IN, IS, IWDL, IWEW, IWHZ, K, MOX
C     !DASH
      external IFOMOR, FOMORI, WGIVE, HI, BYE
C
      dimension W(*)
C
C               DL(KM), XHZ(KM), EW(KM)
      dimension DL(*),  XHZ(*),  EW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IWDL  ),(IN( 2),IWHZ  ),(IN( 3),IWEW  )
C
      call HI ('FIMOOR')
C     !BEG
C     (Get, and allocate, W allotment)
      call IFOMOR (IN,IS,MOX,'FIMOOR')
C
      call FOMORI (DL,XHZ,K,EW,W(IWDL),W(IWHZ),W(IWEW))
C
C     (Give back W allotment)
      call WGIVE  (W,'FIMOOR')
C     !END
      call BYE ('FIMOOR')
C
      return
      end

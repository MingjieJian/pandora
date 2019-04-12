      subroutine FAKIR
     $(N,NSHL,WSH,CODSRW,W,DUMP)
C
C     Rudolf Loeser, 1983 Mar 16
C---- Controls Shell-ray weight matrix interpolation.
C     (This is version 2 of FAKIR.)
C     !DASH
      save
C     !DASH
      real*8 CODSRW, W, WSH
      integer IN, IS, IWM1, IWM2, IWP1, IWP2, MOX, N, NSHL
      logical DUMP, KNT
C     !DASH
      external NAUGHTD, LOON, STORK, WGIVE, HI, BYE
C
      dimension W(*)
C
C               WSH(N,N,NSHL), CODSRW(NSHL)
      dimension WSH(*),        CODSRW(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IWP1  ),(IN( 2),IWP2  ),(IN( 3),IWM1  ),(IN( 4),IWM2  )
C
      call HI ('FAKIR')
C     !BEG
      call NAUGHTD (CODSRW, 1, NSHL, KNT)
      if(.not.KNT) then
C       (Get, and allocate, W allotment)
        call LOON  (IN, IS, MOX, 'FAKIR', N)
C
        call STORK (N, NSHL, WSH, CODSRW, W(IWP1), W(IWP2), W(IWM1),
     $              W(IWM2), DUMP)
C
C       (Give back W allotment)
        call WGIVE (W, 'FAKIR')
      end if
C     !END
      call BYE ('FAKIR')
C
      return
      end

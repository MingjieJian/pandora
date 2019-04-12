      subroutine TOURMAL
     $(XND,N,NL,LU,W,IW)
C
C     Rudolf Loeser, 1999 Nov 08
C---- Supervises the plot of level-population ranks.
C     !DASH
      save
C     !DASH
      real*8 W, XND
      integer IINN, IIPNT, IIWRK, IN, IRND, IS, IW, IWS, IXX, IZN, JN,
     $        LU, MOX, MUX, N, NL
C     !DASH
      external LAZULI, MILETUS, ROMAIN, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XND(N,NL)
      dimension XND(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXX   ),(IN( 2),IRND  ),(IN( 3),IZN   )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IINN  ),(JN( 2),IIPNT ),(JN( 3),IIWRK )
C
      call HI ('TOURMAL')
C     !BEG
      if(LU.gt.0) then
C       (Get, and allocate, W & IW allotments)
        call LAZULI  (IN, IS , MOX, 'TOURMAL')
        call MILETUS (JN, IWS, MUX, 'TOURMAL')
C
        call ROMAIN  (XND, N, NL, LU, W(IXX), W(IRND), W(IZN), IW(IINN),
     $                IW(IIPNT), IW(IIWRK))
C
C       (Give back W & IW allotments)
        call WGIVE   (W , 'TOURMAL')
        call IGIVE   (IW, 'TOURMAL')
      end if
C     !END
      call BYE ('TOURMAL')
C
      return
      end

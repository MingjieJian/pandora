      subroutine DANZIG
     $(DODIDH,N,Z,WS,SNU,DIDH,IMX,W)
C
C     Rudolf Loeser, 2000 May 18
C---- Sets up calculation of smoothed dI/dh, at a given wavelength.
C     Also returns IMX, the index of the maximum value.
C     (This is version 2 of DANZIG.)
C     !DASH
      save
C     !DASH
      real*8 DIDH, SNU, W, WS, Z
      integer ID, IMX, IN, IS, IX, MOX, N
      logical DODIDH
C     !DASH
      external IDANZ, ZIGAND, WGIVE, HI, BYE
C
      dimension W(*)
C
C               Z(N), WS(N), SNU(N), DIDH(N)
      dimension Z(*), WS(*), SNU(*), DIDH(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IX    ),(IN( 2),ID    )
C
      call HI ('DANZIG')
C     !BEG
      if(DODIDH) then
C       (Get, and allocate, W allotmens)
        call IDANZ  (IN, IS, MOX, 'DANZIG')
C
        call ZIGAND (N, Z, WS, SNU, W(IX), W(ID), DIDH, IMX)
C
C       (Give back W allotment)
        call WGIVE  (W, 'DANZIG')
C
      else
        IMX = 0
      end if
C     !END
      call BYE ('DANZIG')
C
      return
      end

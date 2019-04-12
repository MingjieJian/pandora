      subroutine RASAR
     $(N,NW,MUX,BRIGHT,COMK)
C
C     Rudolf Loeser, 1998 Jun 08
C---- Packs the Spectrum Summary sort key.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, COMK, F, XMUX
      integer I, MUX, MUXI, N, NW
C     !DASH
      external HI, BYE
C
C               MUX(NW), BRIGHT(NW), COMK(NW)
      dimension MUX(*),  BRIGHT(*),  COMK(*)
C
      data F /1.D+10/
C
      call HI ('RASAR')
C     !BEG
      do 100 I = 1,NW
        MUXI = MUX(I)
        if((MUXI.lt.1).or.(MUXI.gt.N)) then
          MUXI = 2*N
        end if
C
        XMUX = MUXI
        COMK(I) = (XMUX*F)-BRIGHT(I)
  100 continue
C     !END
      call BYE ('RASAR')
C
      return
      end

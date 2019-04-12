      subroutine JOKE
     $(N,PHI,GTNUL,SLUL,COPUL,BCUL,S)
C
C     Rudolf Loeser, 1984 Jan 17
C---- Computes Line Source Function for passive transitions.
C     (This is version 2 of JOKE.)
C     !DASH
      save
C     !DASH
      real*8 BCUL, COPUL, GTNUL, PHI, S, SLUL, XDEN, XKPL, XNUM
      integer I, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               PHI(N), GTNUL(N), SLUL(N), COPUL(N), BCUL(N), S(N)
      dimension PHI(*), GTNUL(*), SLUL(*), COPUL(*), BCUL(*), S(*)
C
      call HI ('JOKE')
C     !BEG
      do 100 I = 1,N
        XKPL = PHI(I)*GTNUL(I)
        XDEN = (XKPL*SLUL(I)+COPUL(I)*BCUL(I))
        XNUM = (XKPL        +COPUL(I)        )
C
        call DIVIDE (XDEN,XNUM,S(I))
  100 continue
C     !END
      call BYE ('JOKE')
C
      return
      end

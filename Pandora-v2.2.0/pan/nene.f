      subroutine NENE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1982 Jun 22
C---- Computes gas parameters,
C     adjusts NH for constant pressure (if needed), and
C     prints results.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, ZUNIGA, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /28/
C
      call HI ('NENE')
C     !BEG
      call LOGIN  (NPROG)
      call ZUNIGA (X, W, IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('NENE')
C
      return
      end

      subroutine TAY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 25
C---- Reads last part of input.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, BUSY, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data      NPROG /2/
C
      call HI ('TAY')
C     !BEG
      call LOGIN  (NPROG)
      call BUSY   (X,IX,W,IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('TAY')
C
      return
      end

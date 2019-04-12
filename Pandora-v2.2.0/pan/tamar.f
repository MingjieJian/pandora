      subroutine TAMAR
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 May 10
C---- Subiteration processing.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, WALTZ, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /13/
C
      call HI ('TAMAR')
C     !BEG
      call LOGIN  (NPROG)
      call WALTZ  (X,IX,W,IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('TAMAR')
C
      return
      end

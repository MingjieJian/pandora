      subroutine THURSO
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jun 25
C---- Drives Continuum Summaries and Data Save File.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, FREIA, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /20/
C
      call HI ('THURSO')
C     !BEG
      call LOGIN  (NPROG)
      call FREIA  (X, W)
      call LOGOUT (NPROG)
C     !END
      call BYE ('THURSO')
C
      return
      end

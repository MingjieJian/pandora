      subroutine COLNE
     $(KCOMP,KPRNT,KPLOT,X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 15
C---- Number Densities and Departure Coefficients.
C     (This is version 2 of COLNE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, KCOMP, KPLOT, KPRNT, NPROG
C     !DASH
      external LAOS, LOGIN, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /17/
C
      call HI ('COLNE')
C     !BEG
      call LOGIN  (NPROG)
      call LAOS   (KCOMP, KPRNT, KPLOT, X, IX, W, IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('COLNE')
C
      return
      end

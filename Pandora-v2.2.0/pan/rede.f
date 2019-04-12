      subroutine REDE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 21
C---- Drives initialization of BD, ND, NK & SET.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IOVER, IW, IX, NPROG
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C     !DASH
      external LOGIN, INGER, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /12/
C
      call HI ('REDE')
C     !BEG
      if(IOVER.eq.1) then
        call LOGIN  (NPROG)
        call INGER  (X, IX, W, IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('REDE')
C
      return
      end

      subroutine ARION
     $(X,IX,W,IW,PRNT)
C
C     Rudolf Loeser, 2004 May 26
C---- Drives SIGH.
C     (This is version 2 of ARION.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, JJNK, JJXND, KDZIN
      logical PRNT
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 59),JJXND)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(76),KDZIN)
C     !DASH
      external SIGH, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('ARION')
C     !BEG
C---- Compute NDs and NK
      call SIGH (X, W, X(JJXND), X(JJNK), PRNT)
C
C---- Reset ND-signal for diffusion (subroutine TURBOT)
      KDZIN = 0
C     !END
      call BYE ('ARION')
C
      return
      end

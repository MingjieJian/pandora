      subroutine URSULA
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 11
C---- Updates electron density.
C     (This is version 3 of URSULA.)
C     !DASH
      save
C     !DASH
      real*8 W, X, dummy
      integer IETA, IHNK, IHNKR, IN, IS, IW, IX, IXPBL, IZHEL, JJAEL,
     $        JJHND, JJRZM, JJXNC, JJXNE, JJZ, JJZME, JJZRN, MO, MOX, N,
     $        jummy
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ( 80),JJAEL)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ(236),JJZRN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external ILSE, POPIO, WGIVE, ASTRID, TOPAZ, BULLET, WENDY, CHIRP,
     $         POPUTIL, ZERO1, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IETA  ),(IN( 2),IXPBL ),(IN( 3),IHNK  ),(IN( 4),IZHEL ),
     $(IN( 5),IHNKR )
C
      data DUMP /.false./
C     !EJECT
C
      call HI ('URSULA')
C     !BEG
C     (Get, and allocate, W allotment)
      call ILSE    (IN, IS, MOX, 'URSULA')
C     (Initialize populations buffer)
      call POPIO   ('INIT', jummy, W(IXPBL))
C
C---- Get ionized hydrogen number density
      call POPUTIL (W(IXPBL), 1, 0, dummy, 1, W(IHNK), 0, dummy, 0,
     $              dummy)
C---- Get Helium electrons
      call CHIRP   (W, W(IXPBL), W(IZHEL))
C
C---- Compute XNE (and ZME and ETA and ZRN and XNC)
      call ZERO1   (W(IHNKR), N)
      call ASTRID  (X, W, X(JJHND), X(JJRZM), X(JJAEL), X(JJXNE),
     $              X(JJXNC), X(JJZME), X(JJZRN), W(IETA), W(IHNK),
     $              W(IHNKR), W(IZHEL), jummy, MO, DUMP)
C---- Print electrons contributions analysis
      call BULLET  (X, W, IW, X(JJRZM), X(JJHND), X(JJXNE), W(IZHEL),
     $              X(JJZME), X(JJAEL), W(IHNK), X(JJZ), 0)
C
C---- Save for iterative summary
      call TOPAZ   (X(JJXNE), 1)
C---- Continuum Recalculation control
      call WENDY   (X(JJXNE), 1, N, 3, 'URSULA')
C
C     (Give back W allotment)
      call WGIVE   (W, 'URSULA')
C     !END
      call BYE ('URSULA')
C
      return
      end

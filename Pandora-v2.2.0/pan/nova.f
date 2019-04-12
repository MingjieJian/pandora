      subroutine NOVA
     $(X,IX,W,IW,IU,IL,KRJ,LAST,DUMP,PE,FE,TIME)
C
C     Rudolf Loeser, 1968 Jan 31
C---- Computes PE and FE for statistical equilibrium calculations.
C     NOVA aka SUPERCOMPLEX.
C
C     See also VAMOS.
C     !DASH
      save
C     !DASH
      real*8 FE, PE, TIME, TIN, TOUT, W, X
      integer IDI, IL, IN, IS, ISM, IU, IW, IX, IXM, IXR, IXS, IZ,
     $        JJBDI, JJCIJ, JJGM, JJKIJ, JJRHO, JJWEI, JJXND, JJYBR,
     $        KKPIJ, KRJ, MOX, N, NL, NSL
      logical DUMP, LAST
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ( 24),JJWEI)
      equivalence (IZOQ( 44),JJBDI)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
C     !DASH
C     !EJECT
      external SECOND, NUBIA, TRIP, SALAD, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               PE(N), FE(N)
      dimension PE(*), FE(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IXM   ),(IN( 2),IXR   ),(IN( 3),IZ    ),(IN( 4),IDI   ),
     $(IN( 5),IXS   ),(IN( 6),ISM   )
C
      call HI ('NOVA')
C     !BEG
      call SECOND (TIN)
C
C     (Get, and allocate, W allotment)
      call NUBIA  (IN, IS , MOX, 'NOVA')
C
      call SALAD  (LAST, KKPIJ)
      call TRIP   (X, IX, W, IW, N, NL, NSL, IU, IL, KRJ, PE, FE,
     $             X(JJGM), X(JJXND), X(JJCIJ), X(KKPIJ), X(JJRHO),
     $             X(JJYBR), IX(JJKIJ), X(JJWEI), X(JJBDI), W(IZ),
     $             W(IXM), W(IXR), W(ISM), W(IDI), W(IXS), DUMP)
C
C     (Give back W allotment)
      call WGIVE  (W, 'NOVA')
C
      call SECOND (TOUT)
      TIME = TOUT-TIN
C     !END
      call BYE ('NOVA')
C
      return
      end

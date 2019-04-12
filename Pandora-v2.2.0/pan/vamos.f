      subroutine VAMOS
     $(X,IX,W,IW,IU,IL,LAST,DUMP,PE,FE,TIME)
C
C     Rudolf Loeser, 2003 Nov 20
C---- Computes PE and FE for statistical equilibrium calculations,
C     using CHI instead of RHO.
C
C     See also NOVA.
C     !DASH
      save
C     !DASH
      real*8 FE, PE, TIME, TIN, TOUT, W, X
      integer IDI, IL, IN, IS, ISM, IU, IW, IX, IXM, IXR, IXS, IZ,
     $        JJCIJ, JJGM, JJQHI, JJQSA, JJQST, JJYBR, KKPIJ, MOX, N,
     $        NL
      logical DUMP, LAST
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ(246),JJQSA)
      equivalence (IZOQ(247),JJQST)
      equivalence (IZOQ( 13),JJYBR)
C     !DASH
C     !EJECT
      external SECOND, ANUBIS, TEACUP, SALAD, WGIVE, HI, BYE
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
      call HI ('VAMOS')
C     !BEG
      call SECOND (TIN)
C
C     (Get, and allocate, W allotment)
      call ANUBIS (IN, IS , MOX, 'VAMOS')
C
C---- Select appropriate PIJ index (with or without GNVL)
      call SALAD  (LAST, KKPIJ)
C
C---- Compute PE(u,l) and FE(u,l) for transition (i.e. IU,IL)
      call TEACUP (X, IX, W, IW, N, NL, IU, IL, PE, FE, X(JJGM),
     $             X(JJCIJ), X(KKPIJ), X(JJQHI), X(JJQSA), X(JJQST),
     $             X(JJYBR), W(IZ), W(IXM), W(IXR), W(ISM), W(IDI),
     $             W(IXS), DUMP)
C
C     (Give back W allotment)
      call WGIVE  (W, 'VAMOS')
C
      call SECOND (TOUT)
      TIME = TOUT-TIN
C     !END
      call BYE ('VAMOS')
C
      return
      end

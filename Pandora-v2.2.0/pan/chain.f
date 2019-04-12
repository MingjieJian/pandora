      subroutine CHAIN
     $(X,IX,W,IW,IU,IL,KRJ,LAST,DUMP,PE,FE,TIME)
C
C     Rudolf Loeser, 1974 Mar 21
C---- The new version of subroutine COMPLEX:
C     statistical equilibrium equations for transition IU/IL.
C     !DASH
      save
C     !DASH
      real*8 FE, PE, TIME, TIN, TOUT, W, X
      integer IARHO, IBD, IGM, IL, IN, IS, IU, IW, IX, IXND, IZ, KRJ,
     $        MOX, N, NL, NSL
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
C     !DASH
      external SECOND, NAIL, HAMMER, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               PE(N), FE(N)
      dimension PE(*), FE(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IARHO ),(IN( 2),IZ    ),(IN( 3),IGM   ),(IN( 4),IXND  ),
     $(IN (5),IBD   )
C
      call HI ('CHAIN')
C     !BEG
      call SECOND (TIN)
C
C     (Get, and allocate, W allotment)
      call NAIL   (IN, IS, MOX, 'CHAIN')
C
      call HAMMER (IU, IL, N, NL, NSL, KRJ, LAST, X, IX, W(IARHO),
     $             W(IZ), W(IGM), W(IXND), W(IBD), PE, FE, DUMP)
C
C     (Give back W allotment)
      call WGIVE  (W, 'CHAIN')
C
      call SECOND (TOUT)
      TIME = TOUT-TIN
C     !END
      call BYE ('CHAIN')
C
      return
      end

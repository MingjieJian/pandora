      subroutine GRUNCH
     $(X,IX,W,IW,IU,IL,LEVEL,KRJ,LAST,DUMP,PE,FE,TIME)
C
C     Rudolf Loeser, 1974 Mar 22
C---- Drives COMPLEX.
C     (This is version 2 of GRUNCH.)
C     !DASH
      save
C     !DASH
      real*8 FE, PE, TIME, TIN, TOUT, W, X
      integer IFSAR, IFSZ, IL, IN, IPSAR, IPSZ, IS, IU, IW, IX, IZ,
     $        JJBDI, JJCIJ, JJGM, JJKIJ, JJRHO, JJWEI, JJYBR, KKPIJ,
     $        KRJ, LEVEL, MOX, N, NL
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
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ( 24),JJWEI)
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ(145),JJCIJ)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
C     !DASH
      external SECOND, SMELT, COMPLEX, SALAD, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               PE(N), FE(N)
      dimension PE(*), FE(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IZ    ),(IN( 2),IPSZ  ),(IN( 3),IPSAR ),(IN( 4),IFSZ  ),
     $(IN( 5),IFSAR )
C     !EJECT
C
      call HI ('GRUNCH')
C     !BEG
      call SECOND  (TIN)
C
C     (Get, and allocate, W allotment)
      call SMELT   (IN, IS, MOX, 'GRUNCH')
C
      call SALAD   (LAST, KKPIJ)
      call COMPLEX (IU, IL, LEVEL, N, NL, KRJ, X(JJRHO), X(JJYBR),
     $              X(JJBDI), IX(JJKIJ), X(JJWEI), X(JJGM), X(JJCIJ),
     $              X(KKPIJ), X, IX, PE, FE, W(IZ), W(IPSZ), W(IPSAR),
     $              W(IFSZ), W(IFSAR), DUMP)
C
C     (Give back W allotment)
      call WGIVE   (W, 'GRUNCH')
C
      call SECOND  (TOUT)
      TIME = TOUT-TIN
C     !END
      call BYE ('GRUNCH')
C
      return
      end

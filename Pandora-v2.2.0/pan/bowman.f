      subroutine BOWMAN
     $(X,W,N,NL,BDI,BDIJ)
C
C     Rudolf Loeser, 2001 Dec 12
C---- Supervises the direct calculation of b's.
C     (This is version 2 of BOWMAN.)
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIJ, W, X
      integer IESG, IN, IS, IVEC, JJNK, JJXND, MOX, N, NL
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 59),JJXND)
C     !DASH
      external MANBOW, BURION, HYSSOP, BLUET, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               BDI(N,NL), BDIJ(N,NL)
      dimension BDI(*),    BDIJ(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IESG  ),(IN( 2),IVEC  )
C
      call HI ('BOWMAN')
C     !BEG
C     (Get, and allocate, W allotment)
      call MANBOW (IN, IS, MOX, 'BOWMAN')
C
C---- Get ESG1
      call BURION (X, 1, 1, W(IESG))
C---- Get b's
      call HYSSOP (N, NL, BDI, BDIJ, X(JJXND), X(JJNK), W(IESG),
     $             W(IVEC))
C---- ( ? dump)
      call BLUET  (N, BDI, X(JJXND), X(JJNK), W(IESG))
C
C     (Give back W allotment)
      call WGIVE  (W, 'BOWMAN')
C     !END
      call BYE ('BOWMAN')
C
      return
      end

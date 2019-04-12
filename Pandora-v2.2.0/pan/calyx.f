      subroutine CALYX
     $(X,N,NL,ML,ITAU,CHI,ASTAR,SA,A,C)
C
C     Rudolf Loeser, 2003 Mar 14
C---- Supervises the calculation of a and c, for CRYSTAL,
C     !DASH
      save
C     !DASH
      real*8 A, ASTAR, C, CHI, SA, X
      integer ITAU, JJCIJ, JJGM, JJPIJ, ML, N, NL
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ(144),JJPIJ)
C     !DASH
      external LYRA, HI, BYE
C
      dimension X(*)
C
C               ASTAR(N,NT), CHI(N,NT), A(ML,ML), C(ML), SA(N,NT)
      dimension ASTAR(*),    CHI(*),    A(*),     C(*),  SA(*)
C
      call HI ('CALYX')
C     !BEG
      call LYRA (NL, ML, N, ITAU, CHI, ASTAR, SA, X(JJGM), X(JJCIJ),
     $           X(JJPIJ), A, C)
C     !END
      call BYE ('CALYX')
C
      return
      end

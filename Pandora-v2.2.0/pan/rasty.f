      subroutine RASTY
     $(X,AW,SA,ASTAR)
C
C     Rudolf Loeser, 2003 Mar 14
C---- Supervises computation of SA and A* from AW.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, AW, SA, X
      integer JJAIJ, JJALF, JJP, JJSET, JJXNU, N, NL, NNT, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ(253),JJSET)
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 32),JJAIJ)
C     !DASH
      external ZERO1, STARY, HI, BYE
C
      dimension X(*)
C
C               ASTAR(N,NT), AW(N,NT), SA(N,NT)
      dimension ASTAR(*),    AW(*),    SA(*)
C
      call HI ('RASTY')
C     !BEG
      NNT = N*NT
      call ZERO1 (SA   , NNT)
      call ZERO1 (ASTAR, NNT)
C
      call STARY (X(JJXNU), X(JJP), X(JJSET), X(JJALF), X(JJAIJ), AW,
     $            NT, NL, N, SA, ASTAR)
C     !END
      call BYE ('RASTY')
C
      return
      end

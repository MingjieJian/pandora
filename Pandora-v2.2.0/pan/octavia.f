      subroutine OCTAVIA
     $(X,IX)
C
C     Rudolf Loeser, 1986 Jan 23
C---- Drives SNAKE.
C     (This is version 3 of OCTAVIA.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IX, JJCVX, JJFNH, JJHNF, JJISV, JJMU, JJMUF, JJVXN, JJZEC
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  4),JJMU )
      equivalence (IZOQ(115),JJMUF)
      equivalence (IZOQ(164),JJVXN)
      equivalence (IZOQ(186),JJCVX)
      equivalence (IZOQ(226),JJZEC)
      equivalence (IZOQ( 10),JJHNF)
      equivalence (IZOQ( 21),JJFNH)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ( 13),JJISV)
C     !DASH
      external SNAKE, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('OCTAVIA')
C     !BEG
      call SNAKE (X(JJMU), X(JJMUF), X(JJVXN), X(JJCVX), X(JJZEC),
     $            IX(JJISV), X(JJHNF), X(JJFNH))
C     !END
      call BYE ('OCTAVIA')
C
      return
      end

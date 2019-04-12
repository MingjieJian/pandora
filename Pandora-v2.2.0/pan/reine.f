      subroutine REINE
     $(X)
C
C     Rudolf Loeser, 2003 Nov 24
C---- Updates SA and A* using the current number densities.
C     !DASH
      save
C     !DASH
      real*8 X
      integer JJAW, JJQSA, JJQST
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(242),JJAW )
      equivalence (IZOQ(246),JJQSA)
      equivalence (IZOQ(247),JJQST)
C     !DASH
      external RASTY, HI, BYE
C
      dimension X(*)
C
      call HI ('REINE')
C     !BEG
      call RASTY (X, X(JJAW), X(JJQSA), X(JJQST))
C     !END
      call BYE ('REINE')
C
      return
      end

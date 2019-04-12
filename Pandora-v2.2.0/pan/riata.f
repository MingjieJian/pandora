      subroutine RIATA
     $(X,IU,IL,NL,YBRIJ,RATIO)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes the ratio Jbar/alpha.
C     !DASH
      save
C     !DASH
      real*8 RATIO, X, YBRIJ
      integer IL, IU, JJALF, NL
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 25),JJALF)
C     !DASH
      external LASSO, HI, BYE
C
      dimension X(*)
C
C               YBRIJ(NL,NL)
      dimension YBRIJ(*)
C
      call HI ('RIATA')
C     !BEG
      call LASSO (IU, IL, NL, YBRIJ, X(JJALF), RATIO)
C     !END
      call BYE ('RIATA')
C
      return
      end

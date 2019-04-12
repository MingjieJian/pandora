      subroutine CRAWL
     $(LYM,P1,PR,F1,FR,DIV,F,N)
C
C     Rudolf Loeser, 1974 Dec 30
C---- Sets up reserved contributions for printing.
C     !DASH
      save
C     !DASH
      real*8 DIV, F, F1, FR, P1, PR
      integer IP, N
      logical LYM
      character TPOP*3
C     !DASH
      external MOVE1, ARRDIV, CONMUL, HI, BYE
C
C               P1(N), PR(N), F1(N), FR(N), DIV(N)
      dimension P1(*), PR(*), F1(*), FR(*), DIV(*)
C
      call HI ('CRAWL')
C     !BEG
      if(LYM) then
        call MOVE1  (P1, N, F1)
        call ARRDIV (F1, DIV, F1, N)
        call CONMUL (F, F1, N)
C
        call MOVE1  (PR, N, FR)
        call ARRDIV (FR, DIV, FR, N)
        call CONMUL (F, FR, N)
      end if
C     !END
      call BYE ('CRAWL')
C
      return
      end

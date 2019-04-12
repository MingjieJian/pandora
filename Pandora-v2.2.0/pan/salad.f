      subroutine SALAD
     $(REAL,KKPIJ)
C
C     Rudolf Loeser, 1998 Aug 06
C---- Chooses "real" or "non-diffusion" PIJ, for use in the
C     Statistical Equilibrium calculations.
C     !DASH
      save
C     !DASH
      integer JJPIJ, JJTIJ, KKPIJ
      logical REAL
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(144),JJPIJ)
      equivalence (IZOQ(163),JJTIJ)
C     !DASH
      external HI, BYE
C
      call HI ('SALAD')
C     !BEG
      if(REAL) then
        KKPIJ = JJPIJ
      else
        KKPIJ = JJTIJ
      end if
C     !END
      call BYE ('SALAD')
C
      return
      end

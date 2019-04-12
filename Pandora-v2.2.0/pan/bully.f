      subroutine BULLY
     $(INDX,XLM,N,NOPAC,WAVE,ARR,COMP,ALB,BMULT,CONT)
C
C     Rudolf Loeser, 1993 Sep 10
C---- Computes a set of Averaged Line Scattering values.
C     !DASH
      save
C     !DASH
      real*8 ALB, ARR, BMULT, COMP, CONT, WAVE, XLM
      integer INDX, J, KWA, N, NOPAC
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(57),KWA)
C     !DASH
      external VIVIAN, HI, BYE
C
C               WAVE(KWA), ARR(KWA,N), COMP(N), ALB(KWA), CONT(Nopac,N)
      dimension WAVE(*),   ARR(*),     COMP(*), ALB(*),   CONT(NOPAC,*)
C
      call HI ('BULLY')
C     !BEG
      call VIVIAN (XLM, WAVE, ARR, KWA, N, COMP)
C
      do 100 J = 1,N
        CONT(INDX,J) = BMULT*(ALB(J)*COMP(J))
  100 continue
C     !END
      call BYE ('BULLY')
C
      return
      end

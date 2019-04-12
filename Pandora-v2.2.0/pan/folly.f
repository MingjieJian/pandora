      subroutine FOLLY
     $(INDX,XLM,N,NOPAC,WAVEK,ARRK,COMP,ALBK,BMULT,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of Statistical Line Scattering values.
C     (This is version 3 of FOLLY.)
C     !DASH
      save
C     !DASH
      real*8 ALBK, ARRK, BMULT, COMP, CONT, WAVEK, XLM
      integer INDX, J, KNW, N, NOPAC
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(25),KNW)
C     !DASH
      external CLAUDIA, HI, BYE
C
C               WAVEK(KNW), ARRK(KNW,N), COMP(N), ALBK(KNW),
      dimension WAVEK(*),   ARRK(*),     COMP(*), ALBK(*),
C
C               CONT(Nopac,N)
     $          CONT(NOPAC,*)
C
      call HI ('FOLLY')
C     !BEG
      call CLAUDIA (XLM, WAVEK, ARRK, KNW, N, COMP)
C
      do 100 J = 1,N
        CONT(INDX,J) = BMULT*(ALBK(J)*COMP(J))
  100 continue
C     !END
      call BYE ('FOLLY')
C
      return
      end

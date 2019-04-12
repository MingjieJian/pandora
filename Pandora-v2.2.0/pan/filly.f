      subroutine FILLY
     $(INDX,XLM,N,NOPAC,WAVEK,ARRK,COMP,ALBK,BMULT,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of Statistical Line Absorption values.
C     (This is version 3 of FILLY.)
C     !DASH
      save
C     !DASH
      real*8 ALBK, ARRK, BMULT, COMP, CONT, ONE, WAVEK, XLM
      integer INDX, J, KNW, N, NOPAC
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(25),KNW)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external CLAUDIA, HI, BYE
C
C               WAVEK(KNW), ARRK(KNW,N), CONT(Nopac,N), ALBK(KNW),
      dimension WAVEK(*),   ARRK(*),     CONT(NOPAC,*), ALBK(*),
C
C               COMP(N)
     $          COMP(*)
C
      call HI ('FILLY')
C     !BEG
      call CLAUDIA (XLM, WAVEK, ARRK, KNW, N, COMP)
C
      do 100 J = 1,N
        CONT(INDX,J) = BMULT*((ONE-ALBK(J))*COMP(J))
  100 continue
C     !END
      call BYE ('FILLY')
C
      return
      end

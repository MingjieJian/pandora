      subroutine HANSEL
     $(INDX,XLM,N,NOPAC,WAVCO,ARRCO,CLO,ALBK,BMULT,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of Composite Line Absorption values.
C     (This is version 2 of HANSEL.)
C     !DASH
      save
C     !DASH
      real*8 ALBK, ARRCO, BMULT, CLO, CONT, ONE, WAVCO, XLM
      integer INDX, J, N, NCP, NOPAC
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(44),NCP)
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
C               WAVCO(NCP), ARRCO(NCP,N), ALBK(N), CONT(Nopac,N),
      dimension WAVCO(*),   ARRCO(*),     ALBK(*), CONT(NOPAC,*),
C
C               CLO(N)
     $          CLO(*)
C
      call HI ('HANSEL')
C     !BEG
      call CLAUDIA (XLM, WAVCO, ARRCO, NCP, N, CLO)
C
      do 100 J = 1,N
        CONT(INDX,J) = BMULT*((ONE-ALBK(J))*CLO(J))
  100 continue
C     !END
      call BYE ('HANSEL')
C
      return
      end

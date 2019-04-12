      subroutine NIMBLE
     $(XNEST,XNE,FDDL,N)
C
C     Rudolf Loeser, 1992 Feb 06
C---- Computes FDDL, the depth-dependence multiplier for DDL.
C     (This is version 3 of NIMBLE.)
C     !DASH
      save
C     !DASH
      real*8 FDDL, ONE, TWTHRD, XNE, XNEST
      integer I, JDDL, N
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(52),JDDL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(17),TWTHRD)
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SET1, HI, BYE
C
C               XNE(N), FDDL(N)
      dimension XNE(*), FDDL(*)
C
      call HI ('NIMBLE')
C     !BEG
      if(JDDL.le.0) then
        call SET1 (FDDL, N, ONE)
      else
        do 100 I = 1,N
          FDDL(I) = (XNE(I)/XNEST)**TWTHRD
  100   continue
      end if
C     !END
      call BYE ('NIMBLE')
C
      return
      end

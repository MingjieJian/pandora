      subroutine CNIDOS
     $(JLEV,XNU,CP,TR,RK)
C
C     Rudolf Loeser, 1984 Apr 11
C---- Computes a single RK(TR) value, for level JLEV,
C     for the iterative calculation of TREFF.
C     (This is version 3 of CNIDOS.)
C     !DASH
      save
C     !DASH
      real*8 CP, CPJ, F, ONE, PJ, RK, TR, UPJ, XNU, XNUJ, Z
      integer JLEV, LEVS
      logical KOOL, KSHL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external KONE, DIVIDE, FATE, HI, BYE
C
C               CP(NSL+1), XNU(NSL)
      dimension CP(*),     XNU(*)
C
      data LEVS /0/
      data KOOL,KSHL /.false., .false./
C
      call HI ('CNIDOS')
C     !BEG
      XNUJ = XNU(JLEV)
      if(JLEV.ne.LEVS) then
        LEVS = JLEV
C----   Initialize data depending only on level number
        call KONE (XNUJ,KSHL,PJ,Z)
        CPJ = CP(JLEV)
      end if
C---- Now compute RK
      call DIVIDE (Z,TR,UPJ)
      call FATE   (CPJ,XNUJ,PJ,ONE,ONE,UPJ,KOOL,RK,F)
C     !END
      call BYE ('CNIDOS')
C
      return
      end

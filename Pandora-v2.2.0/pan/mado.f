      subroutine MADO
     $(LEVEL,NL,AIJ,XNU,TERM)
C
C     Rudolf Loeser, 1990 Oct 05
C---- Computes a term for GIZON.
C     !DASH
      save
C     !DASH
      real*8 AIJ, ELL2, FAC, ONE, TERM, XNU, ZERO
      integer LEVEL, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               AIJ(NL,NL), XNU(NSL)
      dimension AIJ(NL,*),  XNU(*)
C
      call HI ('MADO')
C     !BEG
      if(LEVEL.gt.1) then
        ELL2 = LEVEL**2
        FAC  = ELL2/((ONE-ONE/ELL2)**2)
        TERM = FAC*(AIJ(LEVEL,1)/XNU(LEVEL))
      else
        TERM = ZERO
      end if
C     !END
      call BYE ('MADO')
C
      return
      end

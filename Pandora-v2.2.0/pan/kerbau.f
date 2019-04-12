      subroutine KERBAU
     $(IU,IL,TE,XNC,AUL)
C
C     Rudolf Loeser, 1990 Oct 10
C---- Computes default value of Hydrogen A, the Einstein A coefficient.
C
C     Johnson, L.C.  1972, ApJ, 174, 227-236.
C     !DASH
      save
C     !DASH
      real*8 AUL, CON70, CQL, ELL2, FLU, ONE, TE, TERM, XNC, YOO2, ZERO
      integer IL, IU
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
      external RIGEL, CUEL, NUKIK, HI, BYE
C
      call HI ('KERBAU')
C     !BEG
      call CUEL    (IL,TE,XNC,CQL)
      if(CQL.le.ZERO) then
        AUL = ZERO
      else
C
        call NUKIK (IU,IL,CQL,FLU)
        call RIGEL (70,CON70)
        ELL2 = IL**2
        YOO2 = IU**2
        TERM = (ELL2/YOO2)*((ONE/ELL2-ONE/YOO2)**2)
C
        AUL = CON70*TERM*FLU
C
      end if
C     !END
      call BYE ('KERBAU')
C
      return
      end

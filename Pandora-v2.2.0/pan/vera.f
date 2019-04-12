      subroutine VERA
     $(PR,CHI,POPK,POP,T,E,FR)
C
C     Rudolf Loeser, 1972 Nov 14
C---- Computes ionized fraction.
C     !DASH
      save
C     !DASH
      real*8 CHI, E, FR, POP, POPK, PR, T, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, FLOCK, HI, BYE
C
      call HI ('VERA')
C     !BEG
      if((POPK.gt.ZERO).and.(POP.gt.ZERO)) then
        call DIVIDE (POPK, POP, FR)
      else
        call FLOCK  (T, E, PR, CHI, FR)
      end if
C     !END
      call BYE ('VERA')
C
      return
      end

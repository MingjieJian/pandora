      subroutine ALARSK
     $(SPHERE,SF,TF,SHL)
C
C     Rudolf Loeser, 1981 Sep 11
C---- Sets up Shell Flux fraction, for printing.
C     !DASH
      save
C     !DASH
      real*8 SF, SHL, TF, ZERO
      logical SPHERE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('ALARSK')
C     !BEG
      if(SPHERE) then
        call DIVIDE (SF,TF,SHL)
      else
        SHL = ZERO
      end if
C     !END
      call BYE ('ALARSK')
C
      return
      end

      subroutine ZIENNA
     $(IFBRSW,DPMULT,DPM)
C
C     Rudolf Loeser, 2003 Jan 16
C---- Sets up the actual Damping Multiplier.
C     !DASH
      save
C     !DASH
      real*8 DPM, DPMULT, ONE
      integer IFBRSW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      call HI ('ZIENNA')
C     !BEG
      if(IFBRSW.eq.1) then
        DPM = ONE
      else
        DPM = DPMULT
      end if
C     !END
      call BYE ('ZIENNA')
C
      return
      end

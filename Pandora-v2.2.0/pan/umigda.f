      subroutine UMIGDA
     $(FLOBRD,CALVAX,VZERO,KINC)
C
C     Rudolf Loeser, 2005 Jan 06
C---- Determines KINC, a component of the velocity code for the
C     line profile calculation.
C     !DASH
      save
C     !DASH
      integer KINC
      logical CALVAX, FLOBRD, VZERO
C     !DASH
      external HI, BYE
C
      call HI ('UMIGDA')
C     !BEG
      if(CALVAX) then
        KINC = 300
      else if(FLOBRD) then
        KINC = 400
      else
        KINC = 100
      end if
      if(.not.VZERO) then
        KINC = KINC+100
      end if
C     !END
      call BYE ('UMIGDA')
C
      return
      end

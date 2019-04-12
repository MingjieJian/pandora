      subroutine POPOUT
     $(BUFFER,LENGTH,IADR)
C
C     Rudolf Loeser, 1982 Jun 02
C---- Writes population data, for POPCORN.
C     !DASH
      save
C     !DASH
      real*8 BUFFER
      integer IADR, LENGTH
C     !DASH
      external CEBU, BOHOL, HI, BYE
C
C               BUFFER(Length)
      dimension BUFFER(*)
C
      call HI ('POPOUT')
C     !BEG
      if(IADR.eq.0) then
        call CEBU  (BUFFER,LENGTH,IADR)
      else
        call BOHOL (BUFFER,LENGTH,IADR)
      end if
C     !END
      call BYE ('POPOUT')
C
      return
      end

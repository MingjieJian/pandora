      subroutine POPIN
     $(BUFFER,LENGTH,IADR)
C
C     Rudolf Loeser, 1982 Jun 02
C---- Reads population data, for POPCORN.
C     !DASH
      save
C     !DASH
      real*8 BUFFER
      integer IADR, LENGTH
C     !DASH
      external LEYTE, POPOK, HI, BYE
C
C               BUFFER(Length)
      dimension BUFFER(*)
C
      call HI ('POPIN')
C     !BEG
      if(IADR.ne.0) then
        call LEYTE (BUFFER,LENGTH,IADR)
        call POPOK (BUFFER)
      end if
C     !END
      call BYE ('POPIN')
C
      return
      end

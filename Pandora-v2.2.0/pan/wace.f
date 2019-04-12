      subroutine WACE
     $(NO,NDW,VSWITCH)
C
C     Rudolf Loeser, 1982 Dec 23
C---- Prints a heading, for BOHUN.
C     !DASH
      save
C     !DASH
      integer NDW, NO
      logical VSWITCH
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      call HI ('WACE')
C     !BEG
      call PRIAM (NO, 'DOPPLER', 7)
C
      call LINER (3, NO)
      write (NO,100) NDW
  100 format(' ','Doppler width, DW, (in Angstroms);'/
     $       ' ','the index of the reference depth NDW =',I3,'.')
      call LINER (1, NO)
C
      if(VSWITCH) then
        write (NO,101)
  101   format(' ','V  is the tangential broadening velocity,'/
     $         ' ','VR is the radial broadening velocity, and'/
     $         ' ','VM is the mean broadening velocity.')
      else
        write (NO,102)
  102   format(' ','V is the broadening velocity.')
      end if
C     !END
      call BYE ('WACE')
C
      return
      end

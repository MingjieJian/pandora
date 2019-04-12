      subroutine OSWALD
     $(NO,NWS,SWAVE)
C
C     Rudolf Loeser, 1992 Aug 04
C---- Prints "subtractional" wavelengths for ABAKAN.
C     !DASH
      save
C     !DASH
      real*8 SWAVE
      integer NO, NWS
C     !DASH
      external DVECOUT, LINER, HI, BYE
C
C               SWAVE(NWS)
      dimension SWAVE(*)
C
      call HI ('OSWALD')
C     !BEG
      if((NO.gt.0).and.(NWS.gt.0)) then
        call LINER   (2,NO)
        write (NO,100)
  100   format(' ','Wavelengths to be deleted from Continuum ',
     $             'Calculations ("subtractional" wavelengths).')
C
        call DVECOUT (NO,SWAVE,NWS,'DELWAVE')
      end if
C     !END
      call BYE ('OSWALD')
C
      return
      end

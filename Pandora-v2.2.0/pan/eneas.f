      subroutine ENEAS
     $(NO,LAB)
C
C     Rudolf Loeser, 1992 Apr 06
C---- Prints labels for DUNLIN.
C     !DASH
      save
C     !DASH
      integer LAB, NO
C     !DASH
      external LINER, HI, BYE
C
      dimension LAB(9)
C
      call HI ('ENEAS')
C     !BEG
      call LINER (1,NO)
C
      if(LAB( 1).eq.1) write (NO,101)
  101 format(' ',10X,'  DW = Doppler width (A)')
C
      if(LAB( 2).eq.1) write (NO,102)
  102 format(' ',10X,'  DP = damping parameter (A)')
C
      if(LAB( 3).eq.1) write (NO,103)
  103 format(' ',10X,'  NE = electron density (/cm**3)')
C
      if(LAB( 4).eq.1) write (NO,104)
  104 format(' ',10X,'FDDL = (NE/NEref)**(2/3) where NEref is printed ',
     $           'under "Summary of Stark Splitting" (option HSTSUMM)'/
     $       ' ',15X,'= multiplier of the DDL values printed under ',
     $           '"Blended Lines Components"')
C
      if(LAB( 5).eq.1) write (NO,105)
  105 format(' ',10X,'  VX = expansion velocity (km/sec)')
C
      if(LAB( 6).eq.1) write (NO,106)
  106 format(' ',10X,'  DV = velocity shift (A)')
C
      if(LAB( 7).eq.1) write (NO,107)
  107 format(' ',10X,'   A = Voigt function parameter')
C
      if(LAB( 8).eq.1) write (NO,108)
  108 format(' ',10X,'   Z = depth (km)')
C
      if(LAB( 9).eq.1) write (NO,109)
  109 format(' ',10X,'  DL = wavelength from line center (A)')
C     !END
      call BYE ('ENEAS')
C
      return
      end

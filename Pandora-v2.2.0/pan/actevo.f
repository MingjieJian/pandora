      subroutine ACTEVO
     $(QELSM,NO,LINES)
C
C     Rudolf Loeser, 2002 Aug 08
C---- Prints blended lines legend, for AVOCET.
C     !DASH
      save
C     !DASH
      integer LDLMX, LINES, NO
      character QELSM*8
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(33),LDLMX)
C     !DASH
      external ABJECT, LINER, HI, BYE
C     !EJECT
C
      call HI ('ACTEVO')
C     !BEG
      call ABJECT  (NO)
      write (NO,100)
  100 format(' ',T10,'Blended Lines components:'///
     $       ' ',T10,'DDL = component wavelength minus the "central" ',
     $           'value, in Angstroms'/
     $       ' ',T10,'DWN = component wavenumber minus the "central" ',
     $           'value, in inverse cm'/
     $       ' ',T10,'CDL = relative component line strengths ',
     $           '(sum = 1.0)'/
     $       ' ',T10,'CRD, CVW, CSK = radiative, van der Waals, and ',
     $           'Stark halfwidths of each component, in Angstroms')
      if((QELSM.eq.'H  ').and.(LDLMX.gt.1)) then
        call LINER (1,NO)
        write (NO,101) LDLMX
  101   format(' ',T10,'Values of DWN and CDL were computed according ',
     $             'to the Hydrogen Stark splitting theory of'/
     $         ' ',T10,'Underhill, A.B. and Waddell, J.H. (1959), ',
     $             'NBS Circular # 603'/
     $         ' ',T10,'The DWN (DDL) values are proportional to ',
     $             'NE**(2/3)'/
     $         ' ',T10,'Whenever the theoretical number of components ',
     $             'of this Stark splitting exceeded  LDLMAX =',I5,
     $             ', then'/
     $         ' ',T10,'"unresolvable" computed components were ',
     $             'combined, and any "minor" components were deleted.'/
     $         ' ',T10,'(More information is printed above when ',
     $             'option HSTSUMM = on.)')
        LINES = LINES+7
      end if
      call LINER   (1,NO)
      LINES = LINES+8
C     !END
      call BYE ('ACTEVO')
C
      return
      end

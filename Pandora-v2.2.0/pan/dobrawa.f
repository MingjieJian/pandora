      subroutine DOBRAWA
     $(LU,WAVENO)
C
C     Rudolf Loeser, 1994 Nov 07
C---- Writes a legend for ORIGINS and/or CONTRIBUTORS printouts.
C     !DASH
      save
C     !DASH
      real*8 CORMN, CORMX
      integer KOELS, LU
      logical WAVENO
      character LAB*11
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(145),CORMN)
      equivalence (RZQ(146),CORMX)
      equivalence (KZQ(180),KOELS)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('DOBRAWA')
C     !BEG
      if(LU.gt.0) then
        if(WAVENO) then
          LAB = 'wavenumbers'
        else
          LAB = 'wavelengths'
        end if
        write (LU,100) LAB,LAB,CORMN,LAB,CORMX
  100   format(' ','Input parameters CORMIN and CORMAX control the ',
     $             'range of ',A11,' to be analyzed.'/
     $         ' ','If CORMIN is greater than or equal to zero, then ',
     $              A11,' < CORMIN are omitted; ',
     $             'in this run CORMIN =',1PE14.6/
     $         ' ','If CORMAX is greater than or equal to zero, then ',
     $              A11,' > CORMAX are omitted; ',
     $             'in this run CORMAX =',E14.6)
        call LINER (1, LU)
        write (LU,101) KOELS
  101   format(' ','Printout lines that are all blank (i.e. all 0) are ',
     $             'printed if input parameter KOELS = 1;'/
     $         ' ','in this run KOELS =',I5,'.')
      end if
C     !END
      call BYE ('DOBRAWA')
C
      return
      end

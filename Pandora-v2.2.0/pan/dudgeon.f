      subroutine DUDGEON
     $(NO,JDL,LDL,PROGLI)
C
C     Rudolf Loeser, 1991 Jun 21
C---- Prints a label for a profile plot.
C     !DASH
      save
C     !DASH
      real*8 PROGLI
      integer JDL, LDL, NO
C     !COM
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      call HI ('DUDGEON')
C     !BEG
      call LINER (1, NO)
      if(WAVENO) then
        if(JDL.le.0) then
          if(LDL.le.1) then
            write (NO,100) 'Line core',COREWN,PROGLI
  100       format(' ',10X,A,' at ',1PE19.12,' /cm; PROGLI =',E11.4)
          else
            write (NO,100) '"Line core"',COREWN,PROGLI
          end if
        else
          write (NO,101) JDL,LDL,COREWN,PROGLI
  101     format(' ',10X,'Component ',I2,' of ',I2,' of blended line. ',
     $               'Center at ',1PE19.12,' /cm; PROGLI =',E11.4)
        end if
      else
        if(JDL.le.0) then
          if(LDL.le.1) then
            write (NO,102) 'Line core',COREWL,PROGLI
  102       format(' ',10X,A,' at ',1PE19.12,' Angstroms; PROGLI =',
     $                 E11.4)
          else
            write (NO,102) '"Line core"',COREWL,PROGLI
          end if
        else
          write (NO,103) JDL,LDL,COREWL,PROGLI
  103     format(' ',10X,'Component ',I2,' of ',I2,' of blended line. ',
     $               'Center at ',1PE19.12,' Angstroms; ',
     $               'PROGLI =',E11.4)
        end if
      end if
C     !END
      call BYE ('DUDGEON')
C
      return
      end

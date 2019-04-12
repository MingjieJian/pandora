      subroutine EFT
     $(NO,EMU,INCRAD)
C
C     Rudolf Loeser, 1986 Feb 21
C---- Writes a header, for CARINA.
C     !DASH
      save
C     !DASH
      real*8 EMU
      integer NO
      logical INCRAD
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
      external LINER, HI, BYE
C
      call HI ('EFT')
C     !BEG
      if(NO.gt.0) then
        call LINER (3, NO)
        if(INCRAD) then
          write (NO,100)
  100     format(' ',42X,'Radiation from illuminating star included.')
        end if
C
        write (NO,101) EMU
  101   format(' ',7('--------'),'  Mu =',F7.4,'  ',7('--------'))
        call LINER (1, NO)
C
        write (NO,102) WLAB2
  102   format(' ',14X,A2,9X,'WVL',9X,'( F   KS  KI)',4X,'OM',6X,
     $             'I/Hz',8X,'I/A',7X,'TB')
        call LINER (1, NO)
      end if
C     !END
      call BYE ('EFT')
C
      return
      end

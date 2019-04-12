      subroutine SWELL
     $(I,LINES,NO,WAVE,IOVR,IHSE,ILYM,LPRD,KONLIC,NRES,ISLV,CONSWI)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Prints Part 2, for SHARI.
C     (This is version 2 of SWELL.)
C     !DASH
      save
C     !DASH
      real*8 WAVE
      integer I, IHSE, ILYM, IOVR, ISLV, KONLIC, LINES, LPRD, MINES, NO,
     $        NRES
      character CONSWI*(*), LAB*11, REMARK*9
C     !DASH
      external  ABJECT, LINER, TSUNAMI, HI, BYE
      intrinsic mod
C
      dimension LAB(5)
C
      data LAB
     $ /'___01-10___', '___11-20___', '___21-30___', '___31-40___',
     $  '___41-50___'/
C
      call HI ('SWELL')
C     !BEG
      if(LINES.ge.55) then
        call ABJECT (NO)
        write (NO,100) LAB
  100   format(' ',12X,'Wavelength',4X,'Iteration',12X,'Contributors'/
     $         ' ',11X,'(Angstroms)',5X,'OV HS LY',3X,'(Notes)',
     $             2X,A11,2X,A11,2X,A11,2X,A11,2X,A11)
        LINES = 2
        MINES = 0
      end if
      if(I.gt.0) then
        if(mod(MINES,5).eq.0) then
          call LINER  (1, NO)
          LINES = LINES+1
        end if
      end if
C
      call TSUNAMI  (REMARK, NRES, ISLV, LPRD, KONLIC, WAVE)
      if(I.gt.0) then
        write (NO,101) I,WAVE,IOVR,IHSE,ILYM,REMARK,CONSWI
  101   format(' ',I7,1PE19.12,3I3,1X,A9,2X,A)
        LINES = LINES+1
        MINES = MINES+1
      else
        write (NO,102) CONSWI
  102   format(' ',15X,'(line-free)',21X,A)
        LINES = LINES+1
      end if
C     !END
      call BYE ('SWELL')
C
      return
      end

      subroutine CREAKER
     $(LU)
C
C     Rudolf Loeser, 2003 Aug 21
C---- Prints a legend, for SHARI.
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external LINER, HI, BYE
C
      call HI ('CREAKER')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100)
  100   format(' ','In the tabulation that follows,')
C
        call LINER (1, LU)
        write (LU,101)
  101   format(' ','"Iteration" tells when the calculations were ',
     $             'last done:'/
     $         ' ','OV  was the value of the overall iteration ',
     $             'counter at that time'/
     $         ' ','HS  was the value of the HSL iteration counter ',
     $             'at that time'/
     $         ' ','LY  was the value of the Lyman iteration ',
     $             'counter at that time'/
     $         ' ','    (value = 0 means: after those iterations ',
     $             'were completed)')
C
        call LINER (1, LU)
        write (LU,102)
  102   format(' ','"Contributors" tells the status of the several ',
     $             'numbered background contributors:'/
     $         ' ','"-" means: not enabled at this wavelength'/
     $         ' ','"X" means: enabled, and nonzero at one or more ',
     $             'depths'/
     $         ' ','"z" means: enabled, but zero at every depth'/
     $         ' ','"." means: status unknown')
      end if
C     !END
      call BYE ('CREAKER')
C
      return
      end

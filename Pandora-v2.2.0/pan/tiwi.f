      subroutine TIWI
     $(LU,METHCOW,METHCOF,KODE)
C
C     Rudolf Loeser, 1992 Sep 15
C---- Prints a header for WEAVE.
C     (This is version 3 of TIWI.)
C     !DASH
      save
C     !DASH
      integer KODE, LU, METHCOF, METHCOW
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      call HI ('TIWI')
C     !BEG
      if((LU.gt.0).and.(KODE.eq.0)) then
        KODE = 1
        call PRIAM (LU,'CO LINES',8)
        call LINER (2,LU)
C
        write (LU,100)
  100   format(' ','CO-lines Opacity data tables.')
        call LINER (2,LU)
C
        if(METHCOW.eq.1) then
          write (LU,101)
  101     format(' ','Wavenumbers and energies from: Farrenq et al., ',
     $               '1991')
        else if(METHCOW.eq.2) then
          write (LU,102)
  102     format(' ','Wavenumbers and energies from: Coxon & ',
     $               'Hajigeorgiou, 1992')
        else if(METHCOW.eq.3) then
          write (LU,103)
  103     format(' ','Wavenumbers and energies from: Goorvitch, 1994')
        end if
C
        if(METHCOF.eq.1) then
          write (LU,104)
  104     format(' ','Oscillator strengths from: Chackerian & ',
     $               'Tipping, 1983')
        else if(METHCOF.eq.2) then
          write (LU,105)
  105     format(' ','Oscillator strengths from: Improved Chackerian, ',
     $               '1993, and Chackerian and Tipping, 1983')
        else if(METHCOF.eq.3) then
          write (LU,106)
  106     format(' ','Oscillator strengths from: Goorvitch, 1994')
        end if
      end if
C     !END
      call BYE ('TIWI')
C
      return
      end

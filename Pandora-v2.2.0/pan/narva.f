      subroutine NARVA
     $(NO,IQLSP,IQLSG,IQCEF,LSFGC,IZERO)
C
C     Rudolf Loeser, 1991 Oct 04
C---- Prints a legend for ATOM.
C     (This is version 2 of NARVA.)
C     !DASH
      save
C     !DASH
      integer IQCEF, IQLSG, IQLSP, IZERO, LSFGC, NO, jummy
      character CEF*3, LSG*3, LSP*3
C     !DASH
      external ONOFF, LINER, VARAN, HI, BYE
C
      call HI ('NARVA')
C     !BEG
      if(NO.gt.0) then
        call ONOFF (IQLSP, jummy, LSP)
        call ONOFF (IQLSG, jummy, LSG)
        call ONOFF (IQCEF, jummy, CEF)
C
        call LINER (1, NO)
        write (NO,100)
  100   format(' ','N O T E S')
        call LINER (1, NO)
        call VARAN (NO, IZERO, 'JZATOM')
C
        write (NO,101)
  101   format(' ',6X,'(1) Frequency-dependent source functions are ',
     $             'computed at a number of wavelengths across a ',
     $             'half- or full-line'/
     $         ' ',10X,'profile (note the XI table). The computations ',
     $             'involve "continuum" (more properly: "background") ',
     $             'opacity and'/
     $         ' ',10X,'source function values. The input switch ',
     $             'LSFFDB = Constant means that the line-core-',
     $             'wavelength values of'/
     $         ' ',10X,'these continuum parameters are used at all ',
     $             'wavelengths of the line, while LSFFDB = Varying ',
     $             'means that these'/
     $         ' ',10X,'continuum parameters are computed explicitly ',
     $             'at every wavelength of the line.')
C     !EJECT
        call LINER (1, NO)
        write (NO,102)
  102   format(' ',6X,'(2) Background opacity at line wavelengths ',
     $             '(as at all wavelengths) is computed as a SUM of ',
     $             'various contributors'/
     $         ' ',10X,'(described in the printout section BACKGROUND,',
     $             ' below), including the "Line-background-opacities"',
     $             ' ("Kurucz"'/
     $         ' ',10X,'opacities; see "About PANDORA", Section 9, '
     $             'and printout section BACKGROUND, below).')
        write (NO,103)
  103   format(' ',10X,'Whether or not these "Line-background-',
     $             'opacities" are included in the SUM of background ',
     $             'contributors at'/
     $         ' ',10X,'wavelengths pertaining to Line Source Function',
     $             ' calculations is controlled by input switch ',
     $             'LSFBOC.'/
     $         ' ',10X,'Before being added to SUM, the computed values',
     $             ' of the "Line-background-opacities" are multiplied',
     $             ' by an'/
     $         ' ',10X,'empirical factor, the "Line-background-',
     $             'opacities-multiplier" (input parameter OML).')
        call LINER (1, NO)
        write (NO,104)
  104   format(' ',6X,'(3) The "Line opacity multiplier" (input ',
     $             'parameter OLL) is an empirical factor that has ',
     $             'been introduced for the'/
     $         ' ',10X,'purpose of additional control of the ',
     $             'computed total line opacity, according to:'/
     $         ' ',10X,'"Total-line-opacity" = OLL * GTN * PHI + ',
     $             'Background-opacity.')
         call LINER (1, NO)
         write (NO,105) CEF
  105    format(' ',6X,'(4) Multiplicative factor by which collision ',
     $              'rates are successively increased to eliminate ',
     $              'population inversions'/
     $          ' ',10X,'that cause negative source functions (used ',
     $              'only when CEFACTS = ON; in this run CEFACTS =',A3,
     $              ').')
C     !EJECT
        call LINER (1, NO)
        write (NO,106)
  106   format(' ',6X,'(5) "Full" (LSFTYP = 0) is the default. ',
     $             'Choosing "*Direct*" (LSFTYP = 1) often is better ',
     $             'for optically thin'/
     $         ' ',10X,'lines. (See "About PANDORA", Section 5, ',
     $             'Note 48 for further choices and explanation.)')
        call LINER (1, NO)
        write (NO,107)
  107   format(' ',6X,'(6) "net" (KRATE = 1) is the default. ',
     $             'Choosing "single" (KRATE = 2) often is better ',
     $             'for optically thin lines'/
     $         ' ',10X,'where single radiative rates can have better ',
     $             'properties than the combined net radiative rate.')
        call LINER (1, NO)
        write (NO,108)
  108   format(' ',6X,'(7) For defaults, see "About PANDORA", ',
     $             'Section 5, Note 35.')
        call LINER (1, NO)
        write (NO,109) LSP
  109   format(' ',6X,'(8) The Source Function Printout appears for ',
     $             'those transitions that have "LSFPRINT" = yes ',
     $             '(default = no);'/
     $         ' ',10X,'or for all transitions if overriding option '
     $             'LSFPRNT = ON (in this run LSFPRNT = ',A3,').')
        write (NO,110) LSG
  110   format(' ',10X,'The Source Function Graph appears for ',
     $             'those transitions that have "LSFPRINT" = yes ',
     $             '(default = no);'/
     $         ' ',10X,'or for all transitions if overriding option '
     $             'LSFGRAF = ON (in this run LSFGRAF = ',A3,').')
        write (NO,111) LSFGC
  111   format(' ',10X,'The x-axis of the Source Function Graph is ',
     $             'controlled by LSFGC; in this run LSFGC =',I2,'.')
      end if
C     !END
      call BYE ('NARVA')
C
      return
      end

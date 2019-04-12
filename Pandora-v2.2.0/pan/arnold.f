      subroutine ARNOLD
     $(NO,LLY,XLMXX,XLMDR,XLMZ,XLMH,XLMCR,NLY,IFALL,NQLYM,JHLSK,PMSK,
     $ NFL,WNRMLA,FNRMLA,WNRMLB,FNRMLB,IQLNU)
C
C     Rudolf Loeser, 1978 Feb 14
C---- Prints Lyman Alpha Opacity calculation input.
C     !DASH
      save
C     !DASH
      real*8 FNRMLA, FNRMLB, PMSK, RYDBRG, WNRMLA, WNRMLB, XLMCR, XLMDR,
     $       XLMH, XLMXX, XLMZ
      integer IFALL, IQLNU, JHLSK, LLY, NFL, NLY, NO, NQLYM
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C     !DASH
C     !EJECT
      external PADMA, HUNTER, LINER, GLISTEN, HI, BYE
C
C               WNRMLA(NFL), FNRMLA(NFL), WNRMLB(NFL), FNRMLB(NFL),
      dimension WNRMLA(*),   FNRMLA(*),   WNRMLB(*),   FNRMLB(*),
C
C               XLMXX(LLY), XLMDR(LLY)
     $          XLMXX(*),   XLMDR(*)
C
      call HI ('ARNOLD')
C     !BEG
      if(NO.gt.0) then
        call PADMA     (NO, 'Hydrogen Lyman lines opacity')
        call LINER     (1, NO)
        write (NO,100) RYDBRG,XLMZ,XLMH
  100   format(' ','The contribution to the "background" opacity from ',
     $             'the Hydrogen Lyman (N/1) lines is computed in the ',
     $             'wavelength range'/
     $         ' ',F17.12,' to ',F7.0,', = LMZ (Angstroms), an ',
     $             'input parameter. Outside this range there is no ',
     $             'contribution. Moreover'/
     $         ' ','the contributions from the highest lines, N>15, ',
     $             'are cut off at wavelengths greater than'/
     $         ' ',F7.0,' = LMH (Angstroms), another input parameter.')
        call LINER     (2, NO)
        call HUNTER    (NO, XLMXX, XLMDR, LLY)
        call LINER     (1, NO)
        write (NO,101) NLY
  101   format(' ','The higher Lyman lines limit NLY =',I5)
        call LINER     (1, NO)
        if(IFALL.gt.0) then
          write (NO,102) IFALL
  102     format(' ','IFALL =',I5,': Resonance broadening using ',
     $               'Allard tables.')
        else
          write (NO,103) IFALL,XLMCR
  103     format(' ','IFALL =',I5,': Resonance broadening using ',
     $               'LMCR (= CTR) =',1PE14.6)
        end if
        write (NO,104) NQLYM,JHLSK,PMSK
  104   format(' ','NQLYM =',I5,' is the weighting limit for the ',
     $             'highest Lyman lines (0 means: no weighting).'/
     $         ' ','JHLSK =',I5,': Stark broadening switch (1 means: ',
     $             'use it).'/
     $         ' ','PMSK  =',1PE14.4,': multiplier for computed ',
     $             'default Stark broadening coefficients.')
        if(IQLNU.gt.0) then
C----     Print H Ly lines normalization data
          call GLISTEN (NO, NFL, WNRMLA, FNRMLA, WNRMLB, FNRMLB)
        end if
      end if
C     !END
      call BYE ('ARNOLD')
C
      return
      end

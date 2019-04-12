      subroutine AMALFI
     $(FABD,NO)
C
C     Rudolf Loeser, 1994 Nov 15
C---- Prints list of "background" contributors, with explanations.
C     !DASH
      save
C     !DASH
      real*8 FABD, ONE
      integer I, J1, J2, LTE, NLT, NO, NPA, NSC
      logical KILROY
C     !COM
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C---- FORGO       as of 2007 Jan 12
      integer     MLINCC,NLINCC,LLINCC
      parameter   (MLINCC=18)
      dimension   LLINCC(MLINCC)
      common      /FORGO/ NLINCC,LLINCC
C     List of continuum contributors omitted from "true" continuum
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external PRIAM, LINER, DASHER, FORAGER, PEA, TRAVE, HI, BYE
C
      call HI ('AMALFI')
C     !BEG
      if(NO.gt.0) then
        call PRIAM  (NO, 'BACKGROUND', 10)
        call LINER  (1, NO)
C
C
        LTE = 9
        NPA = 34
C
        NSC = NOPAC-NPA
        NLT = NPA-LTE
C
C     !EJECT
        write (NO, 100)
  100   format(' ','C o n t r i b u t o r s   t o   B a c k g r ',
     $             'o u n d   C o n t i n u u m')
        call DASHER (NO)
        write (NO,101)
  101   format(' ','"Continuum Calculations" are done to obtain the ',
     $             'total background opacity (OPAC), the scattering ',
     $             'ratio (SCAT), the absorption'/
     $         ' ','source function (BHS), the mean intensity (JNU), ',
     $             'and the continuum source function (SC), at ',
     $             'various wavelengths as needed.'/
     $         ' ','These terms enter the calculations of: line ',
     $             'source functions, transition rates, and the ',
     $             'final spectrum.')
        call LINER  (1, NO)
        write (NO,102) NOPAC,NPA,NSC,NOPAC,NLT,LTE,NSC,NPA
  102   format(' ','PANDORA treats ',I2,' separate "contributors" to ',
     $             'these continuum terms. They are listed in the ',
     $             'table below. These contributors are'/
     $         ' ','of two types: ',I2,' "pure absorption" ',
     $             'contributors, and ',I2,' "scattering" ',
     $             'contributors. The program computes the ',
     $             'individual opacities'/
     $         ' ','due to all ',I2,' contributors, and computes ',
     $             'individual source functions for ',I2,' of the ',
     $             'pure absorption contributors, the other ',I2/
     $         ' ','pure absorption contributors are treated in LTE ',
     $             '(i.e. their source functions are the Planck ',
     $             'function, indicated by * in the'/
     $         ' ','table). (For the ',I2,' scattering contributors ',
     $             'the source function is JNU.) The absorption ',
     $             'source function BHS is the absorption-'/
     $         ' ','weighted sum of the individual source functions ',
     $             'due to the ',I2,' pure absorption contributors.')
        call LINER  (1, NO)
        write (NO,103) NSC,NPA,NPA
  103   format(' ','Defining',9X,'TSCA: the sum of the opacities ',
     $             'due to all ',I2,' scattering contributors,'/
     $         ' ',17X,'PABS: the sum of the opacities due to all ',
     $             I2,' pure absorption contributors, and'/
     $         ' ',17X,'NBHS: the sum of [source function  x  ',
     $             'corresponding opacity] from all ',I2,' pure ',
     $             'absorption contributors.'//
     $         ' ','Then',13X,'OPAC = TSCA + PABS'/
     $         ' ',17X,'SCAT = TSCA / PABS'/
     $         ' ',17X,'BHS  = NBHS / PABS')
        call DASHER (NO)
        call LINER  (1, NO)
C--------1---------2---------3---------4---------5---------6---------7--
C     !EJECT
        call FORAGER (NO)
        call LINER   (2, NO)
        write (NO,104)
  104   format(' ','All contributors are "on" by default, except if ',
     $             'other necessary inputs are not provided (see ',
     $             'Notes, below). Each contributor'/
     $         ' ','can be turned "off" explicitly with a "NABS" ',
     $             'statement. Note that "Part 2" of the ',
     $             'WAVELENGTHS printout (option WAVEPRNT and'/
     $         ' ','input parameter IWSMD = 1) tells, for each ',
     $             'wavelength, which contributors are "on".')
        call LINER   (1, NO)
        call DASHER  (NO)
        write (NO,105)
  105   format(' ','When PRD is used to compute a line source ',
     $             'function, the following modified forms of OPAC, ',
     $             'SCAT, and BHS are used:'//
     $         ' ',17X,'OPAC = PABS + TSCA + ZABS'/
     $         ' ',17X,'SCAT = (TSCA + ZSCA) / (PABS + ZSCR)'/
     $         ' ',17X,'BHS  = (NBHS + ZBNM) / (PABS + ZBDN)',//
     $         ' ','where ZABS (= GTN*PHI), ZSCA, ZSCR, ZBNM and ZBDN ',
     $             'are obtained from the overall PRD calculation.')
C--------1---------2---------3---------4---------5---------6---------7--
C     !EJECT
        call LINER  (1, NO)
        call DASHER (NO)
        write (NO,106)
  106   format(' ','When emergent line profiles are printed, the  ',
     $             'R E S I D U A L  intensity, defined as the line ',
     $             'intensity relative to the'/
     $         ' ','intensity computed with zero line opacity, is ',
     $             'obtained by dividing the computed line intensities ',
     $             'by the computed line-free'/
     $         ' ','background intensity distribution. The line-free ',
     $             'background is the same as the normal background ',
     $             'except that PRD-terms (if any)'/
     $         ' ','as well as the following potential contributors ',
     $             'are all excluded:')
C
        call LINER  (1, NO)
        I = 0
  107   continue
          I = I+1
          if(I.le.NLINCC) then
            J1 = LLINCC(I)
            I  = I+1
            if(I.le.NLINCC) then
              J2 = LLINCC(I)
              write (NO,108) J1,CNAME(J1),J2,CNAME(J2)
  108         format(' ',I20,2X,A24,I20,2X,A24)
              goto 107
            else
              write (NO,108) J1,CNAME(J1)
            end if
          end if
        call LINER  (1, NO)
C
        write (NO,109)
  109   format(' ','Each line profile calculation uses the normal ',
     $             'background except when that background would ',
     $             'include a simulation of'/
     $         ' ','the particular line being calculated, in which ',
     $             'case that simulated line is excluded from the ',
     $             'background. This specific'/
     $         ' ','background intensity and the line-free intensity ',
     $             'are printed (option PROCPRNT) in each Line ',
     $             'Profile output section.')
C--------1---------2---------3---------4---------5---------6---------7--
C     !EJECT
        call LINER  (1, NO)
        call DASHER (NO)
        write (NO,200)
  200   format(' ','***  N O T E S :')
        call LINER  (1, NO)
        write (NO,201)
  201   format(' ','* 1  Only one of these contributor pairs is ',
     $             'allowed at a given wavelength: "Averaged" has ',
     $             'priority over "Composite", which'/
     $         ' ','     in turn has priority over "Statistical". The ',
     $             'final values of Ssct(23), Csct(25) and Asct(32) ',
     $             'contain the factor ALB,'/
     $         ' ','     the depth-dependent scattering albedo, while ',
     $             'the final values of Sabs(22), Cabs(24) and ',
     $             'Aabs(31) contain the'/
     $         ' ','     factor (ALB-1).')
        write (NO,202)
  202   format(' ','* 2  The H Rayleigh Sct. opacity is obtained by ',
     $             'interpolation from built-in tables except in the ',
     $             'wavelength range 91.2 to'/
     $         ' ','     142.5 nm where it is calculated as part of ',
     $             'H Ly alpha Sct. Both of these opacities should ',
     $             'be on (or both off) to avoid'/
     $         ' ','     false discontinuities at these wavelengths. ',
     $             'The contribution from H Ly alpha Abs., which ',
     $             'ordinarily should be on'/
     $         ' ','     whenever H Ly alpha Sct. is on, is non-zero ',
     $             'for wavelengths near the center of Ly alpha at ',
     $             '121.567 nm, as determined'/
     $         ' ','     by the absorption fraction DR (see ',
     $             '"Hydrogen Lyman alpha opacity" in printout ',
     $             'section INPUT, below).')
        write (NO,203)
  203   format(' ','* 3  In the case of background calculations for ',
     $             'wavelengths pertaining to Line Source Function ',
     $             'calculations of the'/
     $         ' ','     ion-of-the-run, these contributions (also ',
     $             'referred to as "Kurucz" opacities --- see "About ',
     $             'PANDORA", Section 9)'/
     $         ' ','     can be controlled by input parameters ',
     $             'LSFBOC(u,l) and OML(u,l) (see printout section ',
     $             'ATOM, printed earlier).')
        write (NO,204)
  204   format(' ','* 4  Omitted from the specific background ',
     $             'calculations for these very line transitions.')
C--------1---------2---------3---------4---------5---------6---------7--
C     !EJECT
        write (NO,205)
  205   format(' ','* 5  Note input parameters NCOSW, NCB, LCOA, ',
     $             'LCOB and COLINES.')
        write (NO,206)
  206   format(' ','* 6  Note input parameter KDUST.')
        write (NO,207)
  207   format(' ','* 7  Note input parameters KURIN, KURMI and KURMA ',
     $             '(and option KURPRNT and input parameter LWNT).')
        write (NO,208)
  208   format(' ','* 8  Note input parameters NAB, BANDL and BANDU ',
     $             '(and option KOMPRNT and input parameter LWNT).')
        write (NO,209)
  209   format(' ','* 9  Note option AVELOP ',
     $             '(and option AVOPRNT and input parameter LWNT).')
        write (NO,210)
  210   format(' ','*10  Note input parameter NHTSW.')
C
        KILROY = .true.
        if((KOPAC(24).gt.0).or.(KOPAC(25).gt.0)) then
          if(FABD.ne.ONE) then
            call TRAVE (KILROY, NO)
            call LINER (1, NO)
            write (NO,300) FABD
  300       format(' ','The input parameter FABD =',1PE10.3,
     $                 'multiplies all values of Composite Line data, ',
     $                 'index = 24 and 25.')
          end if
        end if
C--------1---------2---------3---------4---------5---------6---------7--
C
        call LINER (2, NO)
        write (NO,999)
  999   format(' ',89X,'(This text last revised 2005 Jun 23)')
      end if
C
C---- Provide alert messages about ABD/KOPAC inconsistencies
      call PEA     (NO)
C     !END
      call BYE ('AMALFI')
C
      return
      end

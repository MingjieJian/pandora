      subroutine TOMBLEN
C
C     Rudolf Loeser, 2003 Aug 22
C---- Prints a general explanation of LSF calculation output.
C     !DASH
      save
C     !DASH
      integer IQLSG, IQLSP, JSFEX, MO, jummy
      character LSG*3, LSP*3
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
      equivalence (KZQ(208),JSFEX)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(287),IQLSP)
      equivalence (IQQ(250),IQLSG)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external PRIAM, ONOFF, LINER, TREMBLE, HI, BYE
C     !EJECT
C
      call HI ('TOMBLEN')
C     !BEG
      if(MO.gt.0) then
        call PRIAM (MO, 'LINE (U/L)', 10)
        call LINER (2, MO)
C
        write (MO,100)
  100   format(' ','Printout from the Line Source Function (LSF) ',
     $             'calculation depends on many options and switches, ',
     $             'as described in "About PANDORA,"'/
     $         ' ','(especially Section 11). The basic controls are ',
     $             'also summarized in the printout section ATOM, ',
     $             'above (Note 7, Transitions).')
C
        call ONOFF (IQLSP, jummy, LSP)
        call ONOFF (IQLSG, jummy, LSG)
        call LINER (1, MO)
        write (MO,101) LSP,LSG
  101   format(' ','The run-wide options LSFPRNT and LSFGRAF enable ',
     $             'output from all LSF calculations. Note that ',
     $             'options ALL and EVERY control'/
     $         ' ','whether there is output from all iterations or ',
     $             'from the last one only. In this run: LSFPRNT is ',
     $              A,' and LSFGRAF is ',A)
        call LINER (1, MO)
        write (MO,102)
  102   format(' ','The transition-specific switch LSFPRINT(u,l) ',
     $             'overrides the settings of LSFPRNT and LSFGRAF.')
        call LINER (1, MO)
        write (MO,103)
  103   format(' ','Provided that option LSFPRNT is on or the switch ',
     $             'LSFPRINT(u,l) = 1, then many other printouts ',
     $             'for transition (u,l)'/
     $         ' ','can be obtained under option control. For ',
     $             'example:'//
     $         ' ','     SEPRNT, SECOMP: statistical equilibrium ',
     $             'calculation details'//
     $         ' ','     TAUPRNT: optical depth (TAU) calculation ',
     $             'details'//
     $         ' ','     PRDPRNT, JNUPRNT, PRDCOPR: PRD calculation ',
     $             'details')
C     !EJECT
        call LINER   (2, MO)
        write (MO,104)
  104   format(' ','In general, computing the LSF and the associated ',
     $             'terms RHO, JBAR, and CHI involves several major ',
     $             'steps:'//
     $         ' ','- computing statistical equilibrium terms that ',
     $             'account for the effects of other transitions;'/
     $         ' ','- computing optical depths (TAU);'/
     $         ' ','- computing Lambda operators ("weight matrices") ',
     $             'that account for how the entire medium affects ',
     $             'the local radiation;'/
     $         ' ','- computing the integral over all frequency grid ',
     $             'points specified for the line;'/
     $         ' ','- (perhaps) computing the integral over all ',
     $             'relevant rays;'/
     $         ' ','- (perhaps) computing the terms needed for a PRD ',
     $             'solution.')
        call LINER   (1, MO)
        if(JSFEX.eq.0) then
          write (MO,105)
  105     format(' ','Additional information about all this will be ',
     $               'printed if the input parameter JSFEX = 1.')
        else
          write (MO,106)
  106     format(' ','The additional general information appearing ',
     $               'on the next page(s) will be suppressed if the ',
     $               'input parameter JSFEX = 0.')
        end if
C
        call LINER   (1, MO)
        write (MO,200)
  200   format(' ',116X,'2004 Jul 07')
C
C----   General Summary
        call TREMBLE (MO)
      end if
C     !END
      call BYE ('TOMBLEN')
C
      return
      end

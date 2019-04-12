      subroutine TULLAH
     $(NO,ITS,ITMSS)
C
C     Rudolf Loeser, 2003 May 07
C---- Prints an explanation for the CSF printout.
C     !DASH
      save
C     !DASH
      real*8 CSFCT
      integer IQNCJ, ITS, NO, jummy
      character ITMSS*9, TEXT*3
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
      equivalence (RZQ( 69),CSFCT)
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
      equivalence (IQQ(154),IQNCJ)
C     !DASH
      external ONOFF, HI, BYE
C     !EJECT
C
      call HI ('TULLAH')
C     !BEG
      if(NO.gt.0) then
        call ONOFF (IQNCJ, jummy, TEXT)
        write (NO,100) TEXT
  100   format(' ','"Iter:" tells about the attempt to compute JNU ',
     $             '(mean intensity) and S (source function) ',
     $             'iteratively; this calculation'/
     $         ' ','also depends on option USENCJ. When USENCJ = ',
     $             'off, S is computed iteratively, and then JNU ',
     $             'from S and the weight matrix.'/
     $         ' ','When USENCJ = on, JNU is computed from the ',
     $             'weight matrix and JNU. In this run USENCJ = ',A3,
     $             ' (as also shown above).')
        if(ITS.eq.-1) then
          write (NO,101) ITMSS
  101     format(' ','   "',A9,'" means: no iterations were tried ',
     $               'because some |R| > 0.5 (R is scattering ratio).')
        else if(ITS.eq.99) then
          write (NO,102) ITMSS
  102     format(' ','   "',A9,'" means: iterations did not converge.')
        else
          write (NO,103) ITMSS,CSFCT,ITS
  103     format(' ','   "',A9,'" means: convergence to CSFCRIT =',
     $               1PE9.2,' required ',I2,' iterations.')
        end if
      end if
C     !END
      call BYE ('TULLAH')
C
      return
      end

      subroutine CITAUR
     $(NO)
C
C     Rudolf Loeser, 2005 Sep 15
C---- Prints header for CRUMB
C     !DASH
      save
C     !DASH
      integer IQAIW, IQSTA, KMMAX, NO
      character BLANK*1, KMMES*13
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
      equivalence (KZQ(132),KMMAX)
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
      equivalence (IQQ(286),IQAIW)
      equivalence (IQQ( 76),IQSTA)
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      call HI ('CITAUR')
C     !BEG
      if(NO.gt.0) then
        call PRIAM   (NO, 'INTEGRATION', 11)
C
        if(KMMAX.gt.0) then
          write (KMMES,100) KMMAX
  100     format('KMMAX =',I6)
        else
          KMMES = BLANK
        end if
C
        call LINER   (1, NO)
        write (NO,101) KMMES
  101   format(' ',A13,83X,'(Options INTAPRNT and AINTPRNT)')
C
        if(IQAIW.le.0) then
          write (NO,102)
  102     format(' ','Tables of XI (profile frequency sampling ',
     $               'points) and A (integration weights) for'/
     $           ' ','Line Profile integrations (a.k.a. frequency ',
     $               'summations).')
          call LINER (1, NO)
          if(IQSTA.gt.0) then
            write (NO,103)
  103       format(' ','(The "standard" tables have already been ',
     $                 'printed.)')
          else
            write (NO,104)
  104       format(' ','(The "standard" tables can be printed by ',
     $                 'turning option STANDARD on.)')
          end if
        end if
      end if
C     !END
      call BYE ('CITAUR')
C
      return
      end

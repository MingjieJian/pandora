      subroutine SURF
     $(LU,KODE)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Prints headings for Continuum Wavelengths summary.
C     (This is version 3 of SURF.)
C     !DASH
      save
C     !DASH
      integer IQWSP, IWSMD, KODE, LU
      character HEAD*11, TEXT*45
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(151),IWSMD)
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
      equivalence (IQQ(216),IQWSP)
C     !DASH
C     !EJECT
      external PRIAM, LINER, HALT, HI, BYE
C
      call HI ('SURF')
C     !BEG
      if(LU.gt.0) then
        if((KODE.lt.0).or.(KODE.gt.2)) then
          write (MSSLIN(1),100) KODE
  100     format('KODE =',I12,', which is not 0, 1, or 2.')
          call HALT ('SURF', 1)
        end if
C
        write (HEAD,101) KODE
  101   format('WAVE SUMM',I2)
        call PRIAM  (LU, HEAD, 11)
C
        if(KODE.eq.0) then
          if(IQWSP.gt.0) then
            write (TEXT,102)
  102       format('To omit Part 1, use WAVEPRNT = off')
          else
            write (TEXT,103)
  103       format('To get Part 1, use WAVEPRNT = on')
          end if
        else if(KODE.eq.1) then
          if(IWSMD.eq.1) then
            write (TEXT,104)
  104       format('To omit Part 2, set IWSMD = 0')
          else
            write (TEXT,105)
  105       format('To get Part 2, set IWSMD = 1')
          end if
        end if
C
        call LINER  (2, LU)
        write (LU,106) KODE,TEXT
  106   format(' ','Summary of wavelengths for which Continuum ',
     $             'Calculations were done - Part ',I1,'.',5X,A)
        call LINER  (2, LU)
      end if
C     !END
      call BYE ('SURF')
C
      return
      end

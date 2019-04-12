      subroutine MADAI
     $(NO,MODE,TYPE,LABEL)
C
C     Rudolf Loeser, 1983 Jul 14
C---- Prints a header.
C     !DASH
      save
C     !DASH
      integer IQSFS, MODE, NO
      character LABEL*7, TYPE*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      equivalence (IQQ( 31),IQSFS)
C     !DASH
      external PRIAM, ABJECT, LINER, HALT, HI, BYE
C     !EJECT
C
      call HI ('MADAI')
C     !BEG
      if(NO.gt.0) then
        if((MODE.lt.1).or.(MODE.gt.2)) then
          write (MSSLIN(1),100) MODE
  100     format('MODE =',I12,', which is not 1 or 2.')
          call HALT   ('MADAI',1)
        end if
C
        if(MODE.eq.1) then
          call PRIAM  (NO,LABEL,7)
          call LINER  (3,NO)
          write (NO,101) TYPE,LABEL
  101     format(' ','Net ',A,' ',A7,' Rates')
        else
          call ABJECT (NO)
          write (NO,102) TYPE,LABEL
  102     format(' ','Integrated Net ',A,' ',A7,' Rates')
          if(IQSFS.gt.0) then
            write (NO,103)
  103       format(' ','Computed with spherical coordinates')
          end if
        end if
C
        if(LABEL.eq.'HEATING') then
          call LINER  (1,NO)
          write (NO,104)
  104     format(' ','(Negative values are printed so that they can ',
     $               'compared directly with the net radiative ',
     $               'cooling rates.)')
        end if
      end if
C     !END
      call BYE ('MADAI')
C
      return
      end

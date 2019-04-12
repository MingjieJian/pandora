      subroutine RUMOR
     $(MRJ,KOLEV,KK)
C
C     Rudolf Loeser, 1998 Nov 04
C---- Sets up default for KK, length of the Lyman XK table.
C     !DASH
      save
C     !DASH
      integer IQLYM, KK, KOLEV, LL, MRJ
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
      equivalence (IQQ( 13),IQLYM)
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
C               MRJ(NSL+1)
      dimension MRJ(*)
C
      call HI ('RUMOR')
C     !BEG
      if(IQLYM.le.0) then
        KK = 0
      else
        if(KK.le.0) then
          LL = MRJ(KOLEV)+1
          if(LL.gt.0) then
            KK = LL
C
          else
            write (MSSLIN(1),100) KOLEV, MRJ(KOLEV)
  100       format('KOLEV =',I12,', MRJ(KOLEV) =',I12,
     $             '; default value of KK cannot be computed.')
            call HALT ('RUMOR', 1)
          end if
C
        end if
      end if
C     !END
      call BYE ('RUMOR')
C
      return
      end

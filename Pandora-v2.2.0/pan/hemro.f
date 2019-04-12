      subroutine HEMRO
     $(KIJ,NL)
C
C     Rudolf Loeser, 2006 Jun 12
C---- Fiddles with KIJ when OPTHINL = on.
C     !DASH
      save
C     !DASH
      integer IQOTL, KIJ, NL
C     !COM
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
      equivalence (IQQ(344),IQOTL)
C     !DASH
      external ATLAS, HI, BYE
C
C               KIJ(NL,NL)
      dimension KIJ(*)
C
      call HI ('HEMRO')
C     !BEG
      if(IQOTL.gt.0) then
        call ATLAS (KIJ, NL)
      end if
C     !END
      call BYE ('HEMRO')
C
      return
      end

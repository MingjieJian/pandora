      subroutine PETPIJ
     $(NO,N,NL,PIJ,NSL,PIS)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Prints PIJ, and associated stuff.
C     !DASH
      save
C     !DASH
      real*8 PIJ, PIS
      integer IQPIJ, N, NL, NO, NSL
      logical PRNTZ
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
      equivalence (IQQ(279),IQPIJ)
C     !DASH
      external PRIAM, STENO, LINER, OMAR, HI, BYE
C
C               PIJ(N,NL,NL), PIS(N,NL)
      dimension PIJ(*),       PIS(*)
C
      data PRNTZ /.false./
C     !EJECT
C
      call HI ('PETPIJ')
C     !BEG
      if((NO.gt.0).and.(IQPIJ.gt.0)) then
        call PRIAM   (NO,'PIJ',3)
        write (NO,100)
  100   format(' ','PIJ - Bound-free-bound Transition Rates')
        call STENO   (NO,N,NL,PIJ)
        if(NSL.gt.NL) then
          call LINER (2,NO)
          write (NO,101)
  101     format(' ','PIS - Bound-free-bound Transition Rates to ',
     $               'supplementary levels')
          call OMAR  (NO,N,NL,PIS,'Level ',PRNTZ)
        end if
      end if
C     !END
      call BYE ('PETPIJ')
C
      return
      end

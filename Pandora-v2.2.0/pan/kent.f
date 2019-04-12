      subroutine KENT
     $(TMU,N,LG,LABEL,KODE,XMU,TAU,DUMP)
C
C     Rudolf Loeser, 1982 Aug 13
C---- Dumps TMU.
C     !DASH
      save
C     !DASH
      real*8 TAU, TMU, XMU
      integer IQWDD, KODE, LG, LUEO, N
      logical DMPW, DUMP
      character LABEL*100
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ(146),IQWDD)
C     !DASH
C     !EJECT
      external ARROUT, VECOUT, LINER, HI, BYE
C
C               TMU(N,LG), XMU(LG), TAU(N)
      dimension TMU(*),    XMU(*),  TAU(*)
C
      call HI ('KENT')
C     !BEG
      DMPW = IQWDD.gt.0
C
      if(DUMP.or.DMPW) then
        call LINER  (1, LUEO)
        write (LUEO,100) LABEL,KODE
  100   format(' ',A100,I10)
C
        call ARROUT (LUEO, TMU, N, LG, 'TMU')
      end if
C
      if(DUMP) then
        call VECOUT (LUEO, XMU, LG, 'MU'        )
        call VECOUT (LUEO, TAU, N , 'TNU-normal')
      end if
C     !END
      call BYE ('KENT')
C
      return
      end

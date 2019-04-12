      subroutine RHEA
     $(LU,KONT)
C
C     Rudolf Loeser, 1973 Mar 16
C---- Prints a legend on intensity printouts.
C     KONT   =0: Line;   =1: Continuum;   =-1: neither.
C     !DASH
      save
C     !DASH
      integer IQAPP, IQDIC, IQDIL, KONT, LU
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
      equivalence (IQQ(261),IQAPP)
      equivalence (IQQ(289),IQDIL)
      equivalence (IQQ(288),IQDIC)
C     !DASH
      external LINER, HI, BYE
C     !EJECT
C
      call HI ('RHEA')
C     !BEG
      if((LU.gt.0).and.(IQAPP.le.0)) then
        call LINER (1,LU)
C
        write (LU,100)
  100   format(' ','The fraction  F  and the integers  KS  and  KI  ',
     $             'given in parentheses:'//
     $         ' ','     KS is the index of the largest term of ',
     $                      'the sum over depth points by which ',
     $                      'the intensity integral is ',
     $                      'approximated,'/
     $         ' ','        (i.e., generally, it is the depth ',
     $                      'index where most of the ',
     $                      'intensity originates),'//
     $         ' ','     F  is that fractional portion of the ',
     $                      'total sum which is due to the terms ',
     $                      'whose indices are KS-1, KS and KS+1, ',
     $                      'and'//
     $         ' ','     KI is the depth index of the maximum ',
     $                      'dI/dh (the contribution per ',
     $                      'unit height to the intensity).')
C
        if((KONT.eq.0).and.(IQDIL.le.0)) then
          write (LU,101)
  101     format(' ','        (Additional dI/dh output can be ',
     $               'obtained by setting option DIDHL = ON.)')
        else if((KONT.eq.1).and.(IQDIC.le.0)) then
          write (LU,102)
  102     format(' ','        (Additional dI/dh output can be ',
     $               'obtained by setting option DIDHC = ON and ',
     $               'specifying the value of ICDIT.)')
        end if
C
      end if
C     !END
      call BYE ('RHEA')
C
      return
      end

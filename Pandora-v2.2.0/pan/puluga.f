      subroutine PULUGA
     $(N,KDGV,SQS,SLT)
C
C     Rudolf Loeser, 1990 Jan 29
C---- Dumps, for SETPIJ.
C     (This is version 3 of PULUGA.)
C     !DASH
      save
C     !DASH
      real*8 SLT, SQS
      integer IQVDP, KDGV, LUEO, N
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
      equivalence (IQQ(222),IQVDP)
C     !DASH
      external MESHED, VECOUT, MASHED, HI, BYE
C
C               SQS(N), SLT(N)
      dimension SQS(*), SLT(*)
C     !EJECT
C
      call HI ('PULUGA')
C     !BEG
      if((KDGV.ne.0).and.(IQVDP.gt.0)) then
        call MESHED ('PULUGA', 2)
        write (LUEO,100)
  100   format(' ','Debug printout for PIJ/PIS calculation.')
C
        call VECOUT (LUEO, SQS, N, 'SQS: sum over levels of QS'    )
        call VECOUT (LUEO, SLT, N, 'SLT: supplementary levels term')
C
        call MASHED ('PULUGA')
      end if
C     !END
      call BYE ('PULUGA')
C
      return
      end

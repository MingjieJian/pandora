      subroutine RIVALLO
     $(LUG,TRS,XJNUS,TREFF,LIM,Z,N,VEC,TE)
C
C     Rudolf Loeser, 1985 Aug 08
C---- Makes Rates plots.
C     (This is version 2 of RIVALLO.)
C     !DASH
      save
C     !DASH
      real*8 TE, TREFF, TRS, VEC, XJNUS, Z
      integer IQTRP, LIM, LUG, N
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
      equivalence (IQQ(291),IQTRP)
C     !DASH
      external KALPE, KARLIK, HI, BYE
C
C               TRS(N,LIM), XJNUS(N,LIM), TREFF(N,LIM), VEC(N), TE(N),
      dimension TRS(*),     XJNUS(*),     TREFF(*),     VEC(*), TE(*),
C
C               Z(N)
     $          Z(*)
C
      call HI ('RIVALLO')
C     !BEG
      if(LUG.gt.0) then
        call KALPE   (LUG, TE, TRS,   LIM, Z, N, VEC, 'the 1.'   )
        if(IQTRP.gt.0) then
          call KALPE (LUG, TE, TREFF, LIM, Z, N, VEC, 'effective')
        end if
        call KARLIK  (LUG,     XJNUS, LIM, Z, N, VEC)
      end if
C     !END
      call BYE ('RIVALLO')
C
      return
      end

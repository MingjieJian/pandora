      subroutine AARON
     $(N,LDL,CDL,DDL,FDDL,DW,WDL)
C
C     Rudolf Loeser, 1992 Apr 22
C---- Sets up "effective" blended line components weights.
C     !DASH
      save
C     !DASH
      real*8 CDL, CSDW, DDL, DW, ETERM, FDDL, RAT, SUMI, WDL, ZERO
      integer I, IHSSP, IQSTW, L, LDL, N
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
      equivalence (KZQ(136),IHSSP)
      equivalence (RZQ(133),CSDW )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ(304),IQSTW)
C     !DASH
C     !EJECT
      external SET1, DIVIDE, HI, BYE
C
C               CDL(LDL), DDL(LDL), FDDL(N), DW(N), WDL(N,LDL)
      dimension CDL(*),   DDL(*),   FDDL(*), DW(*), WDL(N,*)
C
      call HI ('AARON')
C     !BEG
      do 100 L = 1,LDL
        call SET1         (WDL(1,L), N, CDL(L))
  100 continue
C
      if((IHSSP.gt.0).and.(LDL.gt.1)) then
        if((IQSTW.gt.0).and.(CSDW.ne.ZERO)) then
C
          do 103 I = 1,N
            SUMI = ZERO
            do 101 L = 1,LDL
              call DIVIDE ((FDDL(I)*DDL(L)), (CSDW*DW(I)), RAT)
              ETERM    = exp(-(RAT**2))
              WDL(I,L) = ETERM*WDL(I,L)
              SUMI = SUMI+WDL(I,L)
  101       continue
C
            do 102 L = 1,LDL
              call DIVIDE (WDL(I,L), SUMI, WDL(I,L))
  102       continue
  103     continue
C
        end if
      end if
C     !END
      call BYE ('AARON')
C
      return
      end

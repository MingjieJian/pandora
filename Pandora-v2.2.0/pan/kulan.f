      subroutine KULAN
     $(N,NL,Z,GVL,GVI,GVO,LU,W,IW)
C     Rudolf Loeser, 1989 Oct 20
C---- Sequential smoothing of "Total Terms", for TARPON.
C     !DASH
      save
C     !DASH
      real*8 GVI, GVL, GVO, W, Z
      integer INDX, IPEX, IQDSM, IW, IXASM, J, LU, N, NL, jummy
      logical lummy
      character LABEL*100, TYPE*3
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
      equivalence (KZQ(199),IXASM)
      equivalence (KZQ( 18),IPEX )
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
      equivalence (IQQ(266),IQDSM)
C     !DASH
C     !EJECT
      external SMOOTH, STOAT, MOVE1, AMNESTY, HI, BYE
C
      dimension W(*), IW(*)
C
C               GVL(N,NL), GVI(N), Z(N), GVO(N,NL)
      dimension GVL(N,*),  GVI(*), Z(*), GVO(*)
C
      data      TYPE /'lin'/
C
      call HI ('KULAN')
C     !BEG
      if(IQDSM.gt.0) then
        call MOVE1    (GVL, (N*NL), GVO)
C
        INDX = 0
        if((IPEX.lt.0).or.(IPEX.eq.15)) then
          INDX = IXASM
        end if
C
        do 101 J = 1,NL
          write (LABEL,100) J
  100     format('Total diffusion term GNVL, level ',I2)
          call SMOOTH (Z, GVL(1,J), N, TYPE, LABEL, INDX, W, IW,
     $                 jummy, lummy)
  101   continue
C
        LABEL = 'Total diffusion term GNVI, continuum'
        call SMOOTH   (Z, GVI     , N, TYPE, LABEL, INDX, W, IW,
     $                 jummy, lummy)
C
        call STOAT    (LU, N, NL, GVL, GVI, 'smoothed')
        call AMNESTY  (LU, N, NL, Z, GVL, GVO)
C
      end if
C     !END
      call BYE ('KULAN')
C
      return
      end

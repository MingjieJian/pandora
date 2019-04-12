      subroutine BADGER
     $(N,NL,BDI,XVAL,W,IW)
C
C     Rudolf Loeser, 1992 Jan 30
C---- Controls b-smoothing, for WILY.
C     (This is version 2 of BADGER.)
C     !DASH
      save
C     !DASH
      real*8 BDI, W, XVAL
      integer INDX, IQBSM, IW, J, N, NBS, NL, jummy
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
      equivalence (KZQ(134),NBS  )
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
      equivalence (IQQ(271),IQBSM)
C     !DASH
      external SMOOTH, INDARRD, HI, BYE
C
      dimension W(*), IW(*)
C
C               BDI(N,NL), XVAL(N)
      dimension BDI(N,*),  XVAL(*)
C
      data TYPE,INDX /'lin', 0/
C     !EJECT
C
      call HI ('BADGER')
C     !BEG
      if((IQBSM.gt.0).and.(NL.ge.NBS)) then
C
        call INDARRD  (XVAL, 1, 1, N)
        do 101 J = NBS,NL
          write (LABEL,100) J
  100     format('Departure coefficient, b(',I2,')')
C
          call SMOOTH (XVAL, BDI(1,J), N, TYPE, LABEL, INDX, W, IW,
     $                 jummy, lummy)
  101   continue
C
      end if
C     !END
      call BYE ('BADGER')
C
      return
      end

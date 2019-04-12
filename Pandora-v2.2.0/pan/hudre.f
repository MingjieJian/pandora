      subroutine HUDRE
     $(N,TE,XNE,PF)
C
C     Rudolf Loeser, 2004 Dec 10
C---- Computes Partition Function of the ion-of-the-run.
C     (This is version 3 of HUDRE.)
C     !DASH
      save
C     !DASH
      real*8 PART, PF, TE, XNE
      integer IONST, IQUVP, N
      character QELSM*8
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
      equivalence (RZQ(  5),PART )
      equivalence (KZQ( 56),IONST)
      equivalence (QZQ(  2),QELSM)
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
      equivalence (IQQ(165),IQUVP)
C     !DASH
C     !EJECT
      external MANDOLIN, HI, BYE
C
C               TE(N), XNE(N), PF(N)
      dimension TE(*), XNE(*), PF(*)
C
      call HI ('HUDRE')
C     !BEG
      call MANDOLIN (N, TE, XNE, PF, QELSM, IONST, PART, IQUVP)
C     !END
      call BYE ('HUDRE')
C
      return
      end

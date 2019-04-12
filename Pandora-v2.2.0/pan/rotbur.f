      subroutine ROTBUR
     $(KAMB,KVLG,HE2SIM)
C
C     Rudolf Loeser, 1999 Mar 05
C---- Sets a switch, for TURBOT.
C     !DASH
      save
C     !DASH
      integer IQAN1, KAMB, KVLG, N1MET
      logical HE2, HE2SIM
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
      equivalence (KZQ( 91),N1MET)
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
      equivalence (IQQ(272),IQAN1)
C     !DASH
      external HI, BYE
C
      call HI ('ROTBUR')
C     !BEG
      HE2    = (KAMB.eq.3).or.(KVLG.eq.3)
      HE2SIM = HE2.and.(N1MET.eq.3).and.(IQAN1.gt.0)
C     !END
      call BYE ('ROTBUR')
C
      return
      end

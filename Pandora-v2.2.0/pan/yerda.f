      subroutine YERDA
     $(LUMR,TDST,H2N,CON,TCO)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts miscellaneous data into restart file.
C     !DASH
      save
C     !DASH
      real*8 CON, H2N, TCO, TDST
      integer IQDT2, IQND2, LUMR, MCON, MH2N
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
      equivalence (KZQ( 87),MH2N )
      equivalence (KZQ( 66),MCON )
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
      equivalence (IQQ( 99),IQDT2)
      equivalence (IQQ(212),IQND2)
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
C     !EJECT
      external BUNT, HI, BYE
C
C               TDST(N), H2N(N), CON(N), TCO(N)
      dimension TDST(*), H2N(*), CON(*), TCO(*)
C
      call HI ('YERDA')
C     !BEG
      if((IQDT2.gt.0).and.(IQND2.gt.0)) then
C----   Dust Temperature
        write (LUMR,100) HEAD
  100   format(A80)
        call BUNT (LUMR, TDST, 'TDST')
      end if
      if(MH2N.gt.0) then
C----   H2 number density
        write (LUMR,100) HEAD
        call BUNT (LUMR, H2N, 'H2N')
      end if
      if(MCON.gt.0) then
C----   CO number density
        write (LUMR,100) HEAD
        call BUNT (LUMR, TCO, 'TCO')
        call BUNT (LUMR, CON, 'CON')
      end if
C     !END
      call BYE ('YERDA')
C
      return
      end

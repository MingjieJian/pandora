      subroutine CORON
     $(ITAU,JNEG,KODE)
C
C     Rudolf Loeser, 1996 Jan 16
C---- Sets up debug/error output, for basic b-ratios calculation.
C
C     Returns KODE such that
C     KODE = 0 means: no printout,
C     KODE = 2 means: this is a dump printout, and
C     KODE = 3 means: this is an error advisory printout.
C     !DASH
      save
C     !DASH
      integer IQBRD, ITAU, JNEG, KODE, LDINT, MO
      logical DUMP
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
      equivalence (KZQ( 48),LDINT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      equivalence (IQQ(311),IQBRD)
C     !DASH
C     !EJECT
      external PINNA, HI, BYE
C
      call HI ('CORON')
C     !BEG
      KODE = 0
C
      if(JNEG.gt.0) then
        KODE = 3
C
      else if(IQBRD.gt.0) then
        call PINNA (ITAU, LDINT, DUMP)
        if(DUMP.and.(MO.gt.0)) then
          KODE = 2
        end if
      end if
C     !END
      call BYE ('CORON')
C
      return
      end

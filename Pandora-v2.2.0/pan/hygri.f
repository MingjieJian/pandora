      subroutine HYGRI
     $(WAVCA)
C
C     Rudolf Loeser, 1993 Sep 08
C---- Sets up data for "averaged" line opacity calculation.
C     !DASH
      save
C     !DASH
      real*8 ALOMA, ALOMI, WAVCA, ZERO
      integer IQALO, KAVNP, KAVNT, KAVNZ, KWA, LUKA
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(57),KWA)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(141),KAVNT)
      equivalence (KZQ(142),KAVNP)
      equivalence (KZQ(143),KAVNZ)
      equivalence (RZQ(142),ALOMI)
      equivalence (RZQ(143),ALOMA)
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
      equivalence (IQQ(307),IQALO)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(37),LUKA )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SIMEON, HI, BYE
C
C               WAVCA(KWA)
      dimension WAVCA(*)
C
      call HI ('HYGRI')
C     !BEG
      if((KWA.gt.0).and.(IQALO.gt.0)) then
        call SIMEON (KAVNT,KAVNP,KAVNZ,KWA,WAVCA,ALOMI,ALOMA,LUKA)
C
      else
        KWA   = 0
        KAVNZ = 1
        KAVNT = 1
        KAVNP = 1
        ALOMI = ZERO
        ALOMA = ZERO
      end if
C     !END
      call BYE ('HYGRI')
C
      return
      end

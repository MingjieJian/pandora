      subroutine PEEL
C
C     Rudolf Loeser, 1980 Oct 05
C---- Initializes auxiliary output files.
C     (This is version 2 of PEEL.)
C     !DASH
      save
C     !DASH
      real*8 SMATC, ZERO
      integer IQCCR, IQPDC, IQSCD, JAYTO, KNZGM, LUCR, LUCS, LUEO, LUSM,
     $        MAMAS, NOION, NONC
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
      equivalence (KZQ( 73),MAMAS)
      equivalence (RZQ( 77),SMATC)
      equivalence (KZQ( 94),NOION)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(29),NONC )
      equivalence (LEST(26),KNZGM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(10),LUCR )
      equivalence (LUNITS(18),JAYTO)
      equivalence (LUNITS(27),LUSM )
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS(14),LUCS )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !EJECT
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
      equivalence (IQQ(134),IQSCD)
      equivalence (IQQ(133),IQCCR)
      equivalence (IQQ(179),IQPDC)
C     !DASH
      external DUCK, BUCK, HI, BYE
C
      call HI ('PEEL')
C     !BEG
      if((IQCCR.gt.0).and.(IQSCD.gt.0)) then
C----   For Cooling Rates plot data
        call DUCK (LUCR,  LUEO)
      end if
C
      if((NONC.gt.0).and.(KNZGM.gt.0).and.(NOION.le.0)) then
C----   For P.R.D. restart Jnu
        call DUCK (JAYTO, LUEO)
      end if
C
      if((MAMAS.gt.0).and.(SMATC.gt.ZERO)) then
C----   For matrix samples
        call BUCK (LUSM,  LUEO)
      end if
C
      if(IQPDC.gt.0) then
C----   For debug checksums
        call DUCK (LUCS,  LUEO)
      end if
C     !END
      call BYE ('PEEL')
C
      return
      end

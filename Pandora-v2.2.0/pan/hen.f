      subroutine HEN
     $(XLTIT,LU,IU,IL)
C
C     Rudolf Loeser, 2006 Feb 08
C---- Determines whether to print line center background details.
C     (This is version 2 of HEN.)
C     !DASH
      save
C     !DASH
      real*8 XLTIT
      integer IL, IOVER, IPEX, IQLBD, IU, KAK2, KAK3, KTYPE, LU, LUEO,
     $        MO
      logical CORE
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
      equivalence (IQQ(341),IQLBD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 2),KAK2 )
      equivalence (KAKODS( 3),KAK3 )
      equivalence (KAKODS( 4),KTYPE)
C     !DASH
      external BET, MESHED, MASHED, SUHARD, HI, BYE
C
      call HI ('HEN')
C     !BEG
      LU = 0
      IU = 0
      IL = 0
      if((IQLBD.gt.0).and.(IOVER.gt.0)) then
        call BET      (2, XLTIT)
        CORE = (KTYPE.eq.1).or.(KTYPE.eq.18).or.(KTYPE.eq.24)
        if(CORE) then
          call SUHARD (LU)
          IU = KAK2
          IL = KAK3
        end if
      end if
C
      if((IPEX.lt.0).or.(IPEX.eq.27)) then
        call MESHED   ('HEN', 2)
        write (LUEO,100) IQLBD,MO,IOVER,KTYPE,LU,IU,IL
  100   format(' ','IQLBD =',I2,', MO =',I3,', IOVER =',I3,
     $             ', KTYPE =',I3,', LU =',I3,', IU =',I3,', IL =',I3)
        call MASHED   ('HEN')
      end if
C     !END
      call BYE ('HEN')
C
      return
      end

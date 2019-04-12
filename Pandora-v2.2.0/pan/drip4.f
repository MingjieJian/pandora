      subroutine DRIP4
     $(LU,N,NL,RKC,RLC,DNRTC)
C
C     Rudolf Loeser, 1979 Nov 26
C---- Produces part of the HAWSER printout.
C     !DASH
      save
C     !DASH
      real*8 DNRTC, RKC, RLC
      integer IHSLT, IOMX, IOVER, IQCCR, IQINC, ITHSL, KOLEV, LITER, LU,
     $        LYMIT, N, NL
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
      equivalence (KZQ(  8),IOMX )
      equivalence (KZQ( 20),IHSLT)
      equivalence (KZQ( 19),LYMIT)
      equivalence (KZQ( 33),KOLEV)
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
      equivalence (IQQ(133),IQCCR)
      equivalence (IQQ( 51),IQINC)
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
      equivalence (LEST(19),ITHSL)
      equivalence (LEST(24),LITER)
C     !DASH
      external LINER, VECOUT, HI, BYE
C
C               RKC(N,NL), RLC(N,NL), DNRTC(N)
      dimension RKC(N,*),  RLC(N,*),  DNRTC(*)
C
      logical   KOOL, LAST
C
      call HI ('DRIP4')
C     !BEG
      if(LU.gt.0) then
C
        LAST = ((IOVER*ITHSL*LITER).eq.(IOMX*IHSLT*LYMIT))
        KOOL = ((IQCCR.gt.0).and.LAST)
        if(KOOL) then
          call LINER      (5,LU)
          write (LU,100)
  100     format(' ','Cooling Rates Data')
C
          call VECOUT   (LU,RKC(1,KOLEV),N,'RKC'  )
          call VECOUT   (LU,RLC(1,KOLEV),N,'RLC'  )
          if(IQINC.gt.0) then
            call VECOUT (LU,DNRTC       ,N,'DNRTC')
          end if
        end if
C
      end if
C     !END
      call BYE ('DRIP4')
C
      return
      end

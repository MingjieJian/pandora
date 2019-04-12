      subroutine RATES
     $(X,W,RKI,IQRK,RLI,IQRL,NO)
C
C     Rudolf Loeser, 1979 Nov 26
C---- Supervises the computation of rates from radiation temperatures.
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI, W, X
      integer IN, IOMX, IOVER, IQCCR, IQRK, IQRL, IS, IUP, IUPJ, JH1,
     $        JH2, JJCK, JJCP, JJHJ, JJRKQ, JJRLQ, JJTE, JJTR, JJXNU,
     $        JPOP, MOX, N, NL, NO
      logical KOOL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(136),JJHJ )
      equivalence (IZOQ( 31),JJCK )
      equivalence (IZOQ( 66),JJRKQ)
      equivalence (IZOQ( 67),JJRLQ)
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
      equivalence (KZQ(  8),IOMX )
      equivalence (KZQ( 51),JH1  )
      equivalence (KZQ( 52),JH2  )
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
      equivalence (LEST(22),JPOP )
      equivalence (LEST( 2),IOVER)
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
C     !DASH
      external MATES, SPUR, TALON, CLAW, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               IQRK(NSL), RKI(N,NSL), IQRL(NSL), RLI(N,NSL)
      dimension IQRK(*),   RKI(*),     IQRL(*),   RLI(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IUP   ),(IN( 2),IUPJ  )
C     !EJECT
C
      call HI ('RATES')
C     !BEG
C     (Get, and allocate, W allotment)
      call MATES  (IN,IS,MOX,'RATES')
C
C---- Compute photo-ionization and -recombination rates
      KOOL = .false.
      call SPUR   (N,NL,IQRK,IQRL,X(JJXNU),X(JJCP),X(JJTR),X(JJTE),RKI,
     $             RLI,W(IUP),W(IUPJ),X(JJHJ),KOOL)
C---- Print them
      call TALON  (NO,N,NL,RKI,IQRK,RLI,IQRL,X(JJCK),X(JJHJ),JH1,JH2)
C
C---- Compute and print cooling terms
      KOOL = ((IQCCR.gt.0).and.((IOVER.eq.IOMX).or.(JPOP.eq.0)))
      if(KOOL) then
        call SPUR (N,NL,IQRK,IQRL,X(JJXNU),X(JJCP),X(JJTR),X(JJTE),
     $             X(JJRKQ),X(JJRLQ),W(IUP),W(IUPJ),X(JJHJ),KOOL)
        call CLAW (NO,N,NL,X(JJRKQ),X(JJRLQ))
      end if
C
C     (Give back W allotment)
      call WGIVE  (W,'RATES')
C     !END
      call BYE ('RATES')
C
      return
      end

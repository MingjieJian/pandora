      subroutine NUBA
     $(X,W)
C
C     Rudolf Loeser, 1981 Oct 27
C---- Computes and prints the Z-dependent geometrical quantities
C     required for source function calculations using spherical
C     coordinates.
C     !DASH
      save
C     !DASH
      real*8 R1N, W, X
      integer IQSFS, JJCDK, JJCSH, JJFRR, JJKSR, JJMDK, JJMSH, JJWDK,
     $        JJWSH, JJXDK, JJXSH, JJZ, LFLX, MRR, MSKIP, N, NRPMX,
     $        NSHL, NTAN
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 40),JJXSH)
      equivalence (IZOQ( 60),JJMSH)
      equivalence (IZOQ(109),JJCSH)
      equivalence (IZOQ(169),JJWSH)
      equivalence (IZOQ(153),JJKSR)
      equivalence (IZOQ(135),JJFRR)
      equivalence (IZOQ(132),JJXDK)
      equivalence (IZOQ(133),JJMDK)
      equivalence (IZOQ(134),JJCDK)
      equivalence (IZOQ(168),JJWDK)
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
      equivalence (KZQ( 45),NTAN )
      equivalence (RZQ( 23),R1N  )
      equivalence (KZQ( 61),MSKIP)
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
      equivalence (LEST( 4),NSHL )
      equivalence (LEST( 8),NRPMX)
      equivalence (LEST(15),LFLX )
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
      equivalence (IQQ( 31),IQSFS)
C     !DASH
C     !EJECT
      external YUKAGIR, YURAK, TRINITY, OSTYAK, TOUR, HI, BYE
C
      dimension X(*), W(*)
C
      call HI ('NUBA')
C     !BEG
      if(IQSFS.gt.0) then
C----   "SHELL" quantities
        call YUKAGIR (X(JJZ), X(JJXSH), X(JJMSH), X(JJCSH), X(JJWSH),
     $                LFLX, NSHL, NRPMX)
        call TRINITY (X(JJKSR))
C----   "DISK" quantities
        call YURAK   (X(JJZ), X(JJFRR), X(JJXDK), X(JJMDK), X(JJCDK),
     $                X(JJWDK), LFLX, W)
C----   Print
        call OSTYAK  (X(JJZ), N, NTAN, MSKIP, NSHL, NRPMX, X(JJFRR),
     $                MRR, R1N, X(JJXSH), X(JJMSH), X(JJCSH),
     $                X(JJWSH), X(JJKSR), X(JJXDK), X(JJMDK),
     $                X(JJCDK), X(JJWDK), LFLX)
C----   Checksums
        call TOUR    (X(JJCSH), X(JJWSH), X(JJCDK), X(JJWDK), LFLX)
      end if
C     !END
      call BYE ('NUBA')
C
      return
      end

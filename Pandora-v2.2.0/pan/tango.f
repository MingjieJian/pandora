      subroutine TANGO
     $(WAVES,P,IX)
C
C     Rudolf Loeser, 1980 May 21
C---- Reads  i n i t i a l  portion of input data, initializes the
C     random-access scratch file, the print output file, and the
C     Composite Line Opacity data input file,
C     and does other initializations.
C
C     P, WAVES and IX are working storage (P is used for the Population
C     data block and the "Part-1 to Part-2" input-shuffling block.)
C     (This is version 3 of TANGO.)
C     !DASH
      save
C     !DASH
      real*8 P, WAVES, dummy
      integer IIIBNL, IIIBNU, IQDLA, IX, IXIBNE, IXILR, IXILZA, IXIMR,
     $        IXINLP, IXIPAR, JN, KALOR, KODOUT, KWA, KWC, LUHB, LUIN,
     $        LUWM, MUX, NAB, NL, NO, NOION, NT, NVX, NZM
      character QALHD*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(14),NZM)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(46),KWC)
      equivalence (JZQ(57),KWA)
      equivalence (JZQ(42),NVX)
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
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 65),KALOR)
      equivalence (QZQ(  5),QALHD)
C
C---- BERTH       as of 1990 Nov 20
      integer     LSHF,LADR,ISHF
      dimension   ISHF(7)
      common      /BERTH/ LSHF,LADR,ISHF
C     "Part-1 to Part-2" input shuffling data block.
C     (Allocated by GRUB.)
      equivalence (ISHF( 4),IIIBNL)
      equivalence (ISHF( 5),IIIBNU)
C     !EJECT
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 1),LUIN )
      equivalence (LUNITS(15),LUHB )
      equivalence (LUNITS(23),LUWM )
C
C---- STORPO      as of 2005 Feb 03
      logical     WRLDHO, WRLDPR, WRLDTY
      common      /STORPO/ WRLDHO,WRLDPR,WRLDTY
C     Storage management debug printout control.
C     (See input parameter WORLDLY in Part B.)
C     .
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
      equivalence (IQQ(316),IQDLA)
C     !DASH
C     !EJECT
      external NYMPH, ZEROI, DEFAULT, SHAZAM, MATTEO, DELABRT, ABRAHAM,
     $         FORUM, FENNEL, AMOEBA, MANBUK, HELLO, ATTIC, MAOHA, FOP,
     $         PIPIT, ARGOSY, IMBATH, MUSH, PRIAM, HI, BYE
C
C               WAVES(KMXWAV), P(*), IX(*)
      dimension WAVES(*),      P(*), IX(*)
C
      dimension JN(6)
      equivalence
     $(JN( 1),IXIPAR),(JN( 2),IXINLP),(JN( 3),IXILZA),(JN( 4),IXIBNE),
     $(JN( 5),IXIMR ),(JN( 6),IXILR )
C
      call HI ('TANGO')
C     !BEG
C---- (The next statement may need to be activated for debugging.)
C     call HELLO   (2, 'PANDORA', 0)
C
C---- Set up and initialize storage
      call NYMPH   (JN, MUX)
      call ZEROI   (IX, 1, MUX)
C---- Defaults
      call AMOEBA
      call DEFAULT (1, dummy)
C
C---- Read header line
      read (LUIN,100) HEAD
  100 format(A80)
C
C---- Read first batch of input - counters and options
      call FENNEL  (IX(IXIMR), IX(IXIPAR), IX(IXINLP), IX(IXILZA),
     $              IX(IXILR), P(IIIBNL), P(IIIBNU), IX(IXIBNE), KODOUT)
C
C---- Reset Hi/Bye/Abort system
      call HELLO   (KALOR, QALHD, LUHB)
      if(WRLDPR) then
C----   Initialize storage management dump file
        rewind LUWM
      end if
C
C---- Initialize main printout file, its index?, and error file?
      call ABRAHAM (KODOUT)
C---- Copy header line to appropriate places
      call MUSH
C---- Initialize scratch I/O processing
      call MANBUK
C
C---- Process and print options
      call FOP     (NO)
C     !EJECT
C---- Enter printout header for miscellaneous messages
      call PRIAM    (NO, 'INPUT NOTES', 11)
C
C---- Turn on Delayed ABORT if requested
      if(IQDLA.gt.0) then
        call DELABRT
      end if
C---- Initialize some opacity data files and table lengths
      call MAOHA    (NAB, KWC, KWA)
C
C---- Massage first batch of input
      call FORUM    (NAB, KWC, IX(IXIMR), IX(IXILR), IX(IXILZA),
     $               IX(IXIPAR), P(IIIBNL), P(IIIBNU), WAVES)
C
      if(NOION.le.0) then
C----   Initialize indexing scheme
        call SHAZAM (NL, NT, IX(IXIPAR))
      end if
C---- Save stuff to be shuffled ( M U S T   precede ARGOSY & GUITAR)
      call MATTEO   (P, IX(IXIMR), IX(IXILR), IX(IXILZA), IX(IXIBNE),
     $               IX(IXINLP))
C
C---- Allocate data blocks ( M U S T   precede ARGOSY )
      call ATTIC
C---- Initialize LIMBO
      call PIPIT
C---- Initialize Populations data blocks
      call ARGOSY   (P)
C---- Record storage usage (: special case, before general storage
C     management system has been activated!)
      call IMBATH   (MUX)
C     !END
      call BYE ('TANGO')
C
      return
      end

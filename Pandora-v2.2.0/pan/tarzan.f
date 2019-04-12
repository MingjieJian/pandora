      subroutine TARZAN
     $(KIND,QNAME,INPAIR,MR,LR,LZA,NMT,NTAN,KALOR,BANDL,BANDU,IBNDE,
     $ NOION,KARB,KODOUT,ISCRS,MRTP,MRTPM,LDLMX,KBTMX,KRTMX,KSTMX,
     $ NLPAIR,KMMAX,STOPOPT,NARB,QALHD,KOLEV,IPEX)
C
C     Rudolf Loeser, 1981 Apr 28
C---- Reads miscellaneous input, for FENNEL.
C
C     (Don't forget about LIMKIND!)
C
C     (This is version 3 of TARZAN.)
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, dummy
      integer IBNDE, INPAIR, IPEX, ISCRS, KALOR, KARB, KBTMX, KIND,
     $        KMMAX, KODOUT, KOLEV, KRTMX, KSTMX, LDLMX, LIMKIND, LR,
     $        LZA, MR, MRTP, MRTPM, NAB, NARB, NL, NLPAIR, NMT, NOION,
     $        NSL, NT, NTAN, jummy
      logical STOPOPT
      character QALHD*8, QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(45),NAB)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external CILENTO, BASIL, MUSTARD, FURRY, CHIRON, CENTAUR, EKASHI,
     $         UNMIX, EBONY, WITCH, HALT, DON, HI, BYE
C
C               MRTP(MRTPM), MR(NSL+1), LR(NL), BANDL(NAB), BANDU(NAB),
      dimension MRTP(*),     MR(*),     LR(*),  BANDL(*),   BANDU(*),
C
C               IBNDE(NAB), LZA(*), NLPAIR(2*NL), INPAIR(2*NT)
     $          IBNDE(*),   LZA(*), NLPAIR(*),    INPAIR(*)
C
      data LIMKIND /30/
C     !EJECT
C
      call HI ('TARZAN')
C     !BEG
C
C
      if((KIND.lt.1).or.(KIND.gt.LIMKIND)) then
        write (MSSLIN(1),100) KIND,LIMKIND
  100   format(' ','Error in TARZAN: KIND =',I12,', which is not ',
     $             'between 1 and ',I2,', inclusive.')
        call HALT ('TARZAN', 1)
      end if
C
C
      goto (
C
C        INPAIR         MR        LZA         LR     NLPAIR       OMIT
     $      101,       102,       103,       104,       105,       106,
C
C            DO        USE        NMT       NTAN      KALHD      KALOR
     $      106,       108,       109,       110,       111,       112,
C
C         NOION      BANDL      BANDU      BANDE      KOLEV    WORLDLY
     $      113,       114,       115,       116,       117,       118,
C
C          KARB     OUTPUT      ISCRS       FILE   RUNTOPOP     LDLMAX
     $      119,       120,       121,       122,       123,       124,
C
C         KBTMX      KRTMX      KSTMX      KMMAX       NARB       IPEX
     $      125,       126,       127,       128,       129,       130
C
     $ ), KIND
C     !EJECT
  101 continue
C----   Read definitions of radiative transitions
        call CILENTO (INPAIR, (2*NT), QNAME)
        goto 999
  102 continue
C----   Read MR'S
        call CILENTO (MR, (NSL+1), QNAME)
        goto 999
  103 continue
C----   Read LZA
        call EBONY   (QNAME, LZA)
        goto 999
  104 continue
C----   Read LR'S
        call CILENTO (LR, NL, QNAME)
        goto 999
  105 continue
C----   Read pairs of quantum numbers for each level
        call CILENTO (NLPAIR, (2*NL), QNAME)
        goto 999
  106 continue
C----   Read Options
        call FURRY   ((KIND-5), STOPOPT)
        goto 999
  108 continue
C----   Switch input files
        call CHIRON
        goto 999
  109 continue
C----   Read NMT (= count of metals)
        call WITCH   (QNAME, NMT)
        goto 999
  110 continue
C----   Read NTAN (= ray selection parameter)
        call WITCH   (QNAME, NTAN)
        goto 999
  111 continue
C----   Read KALHD (= Hi/Bye/Abort system control)
        call MUSTARD (QNAME, dummy, jummy, QALHD, 1, 2)
        call UNMIX   (QALHD)
        goto 999
  112 continue
C----   Read KALOR (= Hi/Bye/Abort system control)
        call WITCH   (QNAME, KALOR)
        goto 999
  113 continue
C----   Read no-ion switch
        call WITCH   (QNAME, NOION)
        goto 999
C     !EJECT
  114 continue
C----   Read lower band limits for Composite Line Opacity
        call BASIL   (BANDL, NAB, QNAME)
        goto 999
  115 continue
C----   Read upper band limits for Composite Line Opacity
        call BASIL   (BANDU, NAB, QNAME)
        goto 999
  116 continue
C----   Read band eclipse switches for Composite Line Opacity
        call CILENTO (IBNDE, NAB, QNAME)
        goto 999
  117 continue
C----   Level-N-to-Continuum (Lyman) level index
        call WITCH   (QNAME, KOLEV)
        goto 999
  118 continue
C----   Storage management dump control
        call DON
        goto 999
  119 continue
C----   Read banner-character selector
        call WITCH   (QNAME, KARB)
        goto 999
  120 continue
C----   Read output files code
        call EKASHI  (KODOUT)
        goto 999
  121 continue
C----   Read scratch I/O control
        call WITCH   (QNAME, ISCRS)
        goto 999
  122 continue
C----   Read filespec of "general" input file
        call CENTAUR
        goto 999
  123 continue
C----   Read "Run-to-Pop" indices
        call CILENTO (MRTP, MRTPM, QNAME)
        goto 999
  124 continue
C----   Read LDLMAX
        call WITCH   (QNAME, LDLMX)
        goto 999
C     !EJECT
  125 continue
C----   Read KBTMAX
        call WITCH (QNAME, KBTMX)
        goto 999
  126 continue
C----   Read KRTMAX
        call WITCH (QNAME, KRTMX)
        goto 999
  127 continue
C----   Read KSTMAX
        call WITCH (QNAME, KSTMX)
        goto 999
  128 continue
C----   Read KMMAX
        call WITCH (QNAME, KMMAX)
        goto 999
  129 continue
C----   Read number of banner pages desired
        call WITCH (QNAME, NARB)
        goto 999
  130 continue
C----   Read multipurpose debug switch
        call WITCH (QNAME, IPEX)
        goto 999
C----
  999 continue
C     !END
      call BYE ('TARZAN')
C
      return
      end

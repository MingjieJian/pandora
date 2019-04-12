      subroutine SAFFRON
     $(X,IX,QNAME,J,LZA,ZAUX,INPAIR,KSW,WMNO,WMXO,RHWO,RKWO,W)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Controls the reading of miscellaneous input.
C     !DASH
      save
C     !DASH
      real*8 RHWO, RKWO, W, WMNO, WMXO, X, ZAUX
      integer INPAIR, IX, J, JJKIJ, JJLIJ, JJLRJ, JJMRJ, JJRKC, JJRKH,
     $        JJRLH, JJRRC, JJTKR, JJWEI, JJWRA, JJYKR, JJYRA, JJZ, KSW,
     $        LZA, N, NL, NSL
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 24),JJWEI)
      equivalence (IZOQ( 70),JJRRC)
      equivalence (IZOQ( 77),JJYRA)
      equivalence (IZOQ( 74),JJRKC)
      equivalence (IZOQ(102),JJTKR)
      equivalence (IZOQ(103),JJYKR)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(212),JJRKH)
      equivalence (IZOQ(213),JJRLH)
      equivalence (IZOQ(244),JJWRA)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
      equivalence (JZOQ(  8),JJLIJ)
      equivalence (JZOQ(  1),JJMRJ)
      equivalence (JZOQ(  3),JJLRJ)
C
C---- ZINDEX      as of 1984 Apr 24
      integer     MAUXZI
      common      /ZINDEX/ MAUXZI
C     Auxiliary Z-scale index, for input processing.
C     .
C     !EJECT
C---- LEVDES      as of 1988 May 13
      integer     MILTY,LIMLV
      parameter   (MILTY=50)
C     (Remember to recompile ADAM when changing MILTY.)
      character   LEVDES*8
      dimension   LEVDES(MILTY)
      common      /LEVDES/ LIMLV,LEVDES
C
C     Term designations for levels of the ion of the run.
C     .
C     !DASH
      external  OREGANO, CHIRON, TARACAM, TARAGON, CAPUA, HIGH, SIMMON,
     $          THYMON, GARDON, CORIAND, CENTAUR, CHILI, DAHLIA, WITCH,
     $          METAL, JOJO, CHILKAT, DACIA, URRACA, GLINT, SPUN, SPUD,
     $          PANTS, PHOEBUS, NOTHER, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               ZAUX(LZM,NZM), LZA(50), INPAIR(2*NT), RHWO(N,NT),
      dimension ZAUX(*),       LZA(*),  INPAIR(*),    RHWO(*),
C
C               KSW(Nopac), RKWO(N)
     $          KSW(*),     RKWO(*)
C
      call HI ('SAFFRON')
C     !BEG
      goto (
C
C        FILE     DOSFPRNT WEIGHT   KTRANS   USE      BDOPT    RHOPT
     $   101,     102,     103,     104,     105,     106,     107,
C
C        CIMETHOD ELEMENT  RQCP     LEVDES   WRAT     RRCP     YRATE
     $   108,     109,     110,     111,     112,     113,     114,
C
C        NABS     POPUP    NEWELE   ZAUX     MAUX     RKC      TKR
     $   115,     116,     117,     118,     119,     120,     121,
C
C        YKR      MATRIX   DOPROF   DOFLUX   DOFDB    COLINES  KRATE
     $   122,     123,     124,     125,     126,     127,     128,
C
C        RCHX     XRKH     XRLH     RKW      RHOWT    WMN      WMX
     $   129,     130,     131,     132,     133,     134,     135,
C
C        CEMETHOD
     $   136      ), J
C     !EJECT
  101 continue
C----   Read filespec of "general" input file
        call CENTAUR
        goto 999
  102 continue
C----   Read Line Source Function calculation print switches
        call HIGH    (QNAME, INPAIR)
        goto 999
  103 continue
C----   Read Weights
        call OREGANO (X(JJWEI), QNAME)
        goto 999
  104 continue
C----   Read Transition Type switch
        call GARDON  (NL, QNAME, IX(JJKIJ))
        goto 999
  105 continue
C----   Switch Input files
        call CHIRON
        goto 999
  106 continue
C----   Read BD Option
        call TARACAM
        goto 999
  107 continue
C----   Read RHO Option
        call TARAGON
        goto 999
  108 continue
C----   Read CIMETHOD
        call NOTHER
        goto 999
  109 continue
C----   Read Metals
        call METAL
        goto 999
  110 continue
C----   Read RQCPNJ (=RRCPNJ under another name)
        call CORIAND (QNAME, X(JJRRC), IX(JJMRJ), 2)
        goto 999
  111 continue
C----   Read Level Designations
        call GLINT   (LEVDES, NSL, QNAME)
        goto 999
  112 continue
C----   Read RRNUNJ
        call CORIAND (QNAME, X(JJWRA), IX(JJMRJ), 4)
        goto 999
  113 continue
C----   Read RRCPNJ
        call CORIAND (QNAME, X(JJRRC), IX(JJMRJ), 1)
        goto 999
C     !EJECT
  114 continue
C----   Read YRATE
        call CORIAND (QNAME, X(JJYRA), IX(JJMRJ), 2)
        goto 999
  115 continue
C----   Read Opacity Switches
        call CHILI   (QNAME, KSW)
        goto 999
  116 continue
C----   Read Population Update Switches
        call DACIA   (QNAME)
        goto 999
  117 continue
C----   Read new Metals data
        call SIMMON
        goto 999
  118 continue
C----   Read auxiliary Z Table
        call DAHLIA  (QNAME, LZA, ZAUX)
        goto 999
  119 continue
C----   Read auxiliary Z Table index
        call WITCH   (QNAME, MAUXZI)
        goto 999
  120 continue
C----   Read RKC
        call CORIAND (QNAME, X(JJRKC), IX(JJLRJ), 3)
        goto 999
  121 continue
C----   Read TKR
        call CORIAND (QNAME, X(JJTKR), IX(JJLRJ), 3)
        goto 999
  122 continue
C----   Read YKR
        call CORIAND (QNAME, X(JJYKR), IX(JJLRJ), 3)
        goto 999
  123 continue
C----   Read Matrix parameters
        call CAPUA   (QNAME)
        goto 999
  124 continue
C----   Read profile calculation switches
        call HIGH    (QNAME, INPAIR)
        goto 999
  125 continue
C----   Read line flux calculation switches
        call HIGH    (QNAME, INPAIR)
        goto 999
  126 continue
C----   Read frequency-dependent background switches
        call HIGH    (QNAME, INPAIR)
        goto 999
C     !EJECT
  127 continue
C----   Read CO lines data control indices
        call CHILKAT (QNAME)
        goto 999
  128 continue
C----   Read single rate switch
        call URRACA  (NL, QNAME, IX(JJLIJ))
        goto 999
  129 continue
C----   Charge exchange parameter
        call JOJO    (QNAME)
        goto 999
  130 continue
C----   Charge exchange parameter
        call THYMON  (X(JJRKH), N, QNAME, LZA, ZAUX, X(JJZ), W)
        goto 999
  131 continue
C----   Charge exchange parameter
        call THYMON  (X(JJRLH), N, QNAME, LZA, ZAUX, X(JJZ), W)
        goto 999
  132 continue
C----   Old RK-weights
        call SPUD    (QNAME, RKWO, LZA, ZAUX, X(JJZ), W)
        goto 999
  133 continue
C----   Old RHO-weights
        call SPUN    (QNAME, RHWO, LZA, ZAUX, X(JJZ), W)
        goto 999
  134 continue
C----   Old WMN
        call PANTS   (QNAME, WMNO)
        goto 999
  135 continue
C----   Old WMX
        call PANTS   (QNAME, WMXO)
        goto 999
  136 continue
C----   Read CEMETHOD
        call PHOEBUS
        goto 999
  999 continue
C     !END
      call BYE ('SAFFRON')
C
      return
      end

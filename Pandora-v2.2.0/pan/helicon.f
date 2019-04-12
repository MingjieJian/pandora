      subroutine HELICON
     $(X,W,XPBL1,K1,XPBL2,K2,KODE)
C
C     Rudolf Loeser, 1972 Nov 15
C---- Does the initial processing of "population data"
C     of ions with two stages of ionization.
C     (This is version 3 of HELICON.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XPBL1, XPBL2, dummy
      integer IABDL, ID1, ID2, IN, INDX1, INDX2, IRAB, IS, IX1, IX2,
     $        JJBDI, JJCHI, JJHND, JJNK, JJPFT, JJRAB, JJTE, JJXND,
     $        JJXNE, K1, K2, KLTE1, KLTE2, KODE, LC1, LC2, LLBD, LLPOPK,
     $        LLPOPN, MOX, N, NL, NO, jummy
      logical PRINTB, PRINTN, RUNION1, RUNION2
      character BLANK*1
C     !COM
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
      equivalence (LZOQ( 4),LLPOPN)
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 5),LLBD  )
C     !EJECT
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
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(143),JJCHI)
      equivalence (IZOQ(142),JJPFT)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external  JELLY, NOYON, GOGGLE, PATROL, RABBIT, OLM, ARGIA, SERF,
     $          PUPPET, FRAGA, WGIVE, HI, BYE
      intrinsic min
C
      dimension X(*), W(*)
C
C               XPBL1(Lenpbl), XPBL2(Lenpbl)
      dimension XPBL1(*),      XPBL2(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IX1   ),(IN( 2),IX2   ),(IN( 3),ID1   ),(IN( 4),ID2   ),
     $(IN( 5),IRAB  ),(IN( 6),IABDL )
C
      call HI ('HELICON')
C     !BEG
      if(KODE.gt.0) then
C----   Full calculation needed
C
C       (Get, and allocate, W allotment)
        call JELLY  (IN, IS, MOX, 'HELICON')
C
        RUNION1 = (IUPOP(K1).gt.0)
        RUNION2 = (IUPOP(K2).gt.0)
C----   Set up RAB table
        call RABBIT    ((RUNION1.or.RUNION2), N, X(JJRAB), W(IRAB))
C----   Set up elemental abundance table
        call PATROL (X, POPSYM(K1), INDX1, POPSYM(K2), INDX2,
     $               BLANK, jummy, W(IABDL))
C----   Set up population data
        call NOYON  (N, X(JJTE), X(JJXNE), X(JJCHI), X(JJPFT),
     $               X(JJHND), W(IRAB), W(IABDL),
     $               INDX1, LENPOP(K1), XPBL1(LLPOPN),
     $               XPBL1(LLPOPK), KLTE1, K1, LIMPOP(K1), W(IX1),
     $               W(ID1),
     $               INDX2, LENPOP(K2), XPBL2(LLPOPN),
     $               XPBL2(LLPOPK), KLTE2, K2, LIMPOP(K2), W(IX2),
     $               W(ID2))
C----   Set up departure coefficient data
        call GOGGLE (N, LENPOP(K1), LIMPOP(K1), POPSYM(K1), X(JJTE),
     $               X(JJXNE), X(JJPFT), X(JJCHI), W(IABDL),
     $               K1, XPBL1(LLPOPN), XPBL1(LLPOPK), XPBL1(LLBD),
     $               LC1, 0, dummy)
        call GOGGLE (N, LENPOP(K2), LIMPOP(K2), POPSYM(K2), X(JJTE),
     $               X(JJXNE), X(JJPFT), X(JJCHI), W(IABDL),
     $               K2, XPBL2(LLPOPN), XPBL2(LLPOPK), XPBL2(LLBD),
     $               LC2, 0, dummy)
C     !EJECT
C----   Print for lower stage
        call ARGIA (PRINTN, PRINTB, RUNION1, LIMPOP(K1), N, X(JJNK),
     $              X(JJXND), X(JJBDI), XPBL1(LLPOPK), XPBL1(LLPOPN),
     $              XPBL1(LLBD))
        call SERF  (NO, LENPOP(K1), N, X(JJHND), X(JJXNE),
     $              XPBL1(LLPOPN), XPBL1(LLPOPK), KLTE1, NAMES(K1),
     $              NAMKNT(K1), LIMPOP(K1), K1, IPSWICH(K1),
     $              XPBL1(LLBD), LC1, 1, PRINTN, PRINTB)
C----   Print for upper stage
        call ARGIA (PRINTN, PRINTB, RUNION2, LIMPOP(K2), N, X(JJNK),
     $              X(JJXND), X(JJBDI), XPBL2(LLPOPK), XPBL2(LLPOPN),
     $              XPBL2(LLBD))
        call SERF  (NO, LENPOP(K2), N, X(JJHND), X(JJXNE),
     $              XPBL2(LLPOPN), XPBL2(LLPOPK), KLTE2, NAMES(K2),
     $              NAMKNT(K2), LIMPOP(K2), K2, IPSWICH(K2),
     $              XPBL2(LLBD), LC2, 1, PRINTN, PRINTB)
        if(RUNION1) then
          LENPOP(K1) = min(NL,LIMPOP(K1))
        end if
        if(RUNION2) then
          LENPOP(K2) = min(NL,LIMPOP(K2))
        end if
C
C       (Give back W allotment)
        call WGIVE (W, 'HELICON')
C     !EJECT
      else
C----   No need to compute - just set =0
        call OLM   (N, LIMPOP(K1), XPBL1(LLPOPK), XPBL1(LLPOPN),
     $              XPBL1(LLBD))
        call OLM   (N, LIMPOP(K2), XPBL2(LLPOPK), XPBL2(LLPOPN),
     $              XPBL2(LLBD))
      end if
C
      if(K2.eq.5) then
C----   This is He-II: update charged particle number density
        call FRAGA (X, XPBL2)
      end if
C
C---- Checksums
      call PUPPET  (ICKSM(K1), N, LIMPOP(K1), XPBL1(LLPOPK),
     $              XPBL1(LLPOPN), 1, XPBL1(LLBD))
      call PUPPET  (ICKSM(K2), N, LIMPOP(K2), XPBL2(LLPOPK),
     $              XPBL2(LLPOPN), 1, XPBL2(LLBD))
C     !END
      call BYE ('HELICON')
C
      return
      end

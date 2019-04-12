      subroutine RUBICON
     $(X,W,XPBL,K1,KODE)
C
C     Rudolf Loeser, 1972 Nov 24
C---- Does the initial processing of "population data"
C     of ions with a single stage of ionization.
C     (This is version 3 of RUBICON.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XPBL, dummy
      integer IABDL, IN, INDX, IRAB, IS, IX1, IXD, JJBDI, JJCHI, JJHND,
     $        JJNK, JJPFT, JJRAB, JJTE, JJXND, JJXNE, K1, KLTE, KODE,
     $        LC, LLBD, LLPOPK, LLPOPN, MOX, N, NL, NO, jummy
      logical PRINTB, PRINTN, RUNION
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
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
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
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 59),JJXND)
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
      external  LAMP, NAMUR, GOGGLE, SERF, RABBIT, OLM, ARGIA, PATROL,
     $          PUPPET, WGIVE, HI, BYE
      intrinsic min
C
      dimension X(*), W(*)
C
C               XPBL(Lenpbl)
      dimension XPBL(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IX1   ),(IN( 2),IXD   ),(IN( 3),IRAB  ),(IN( 4),IABDL )
C
      call HI ('RUBICON')
C     !BEG
      if(KODE.gt.0) then
C
C----   Full solution needed
C       (Get, and allocate, W allotment)
        call LAMP   (IN, IS, MOX, 'RUBICON')
C
        RUNION = (IUPOP(K1).gt.0)
C----   Set up RAB table
        call RABBIT (RUNION, N, X(JJRAB), W(IRAB))
C----   Set up element abundance table
        call PATROL (X, POPSYM(K1), INDX, BLANK, jummy, BLANK, jummy,
     $               W(IABDL))
C----   Set up population data
        call NAMUR  (N, X(JJTE), X(JJXNE), X(JJCHI), X(JJPFT),
     $               X(JJHND), W(IRAB), W(IABDL),
     $               INDX, LENPOP(K1), XPBL(LLPOPN), XPBL(LLPOPK),
     $               KLTE, K1, LIMPOP(K1), W(IX1), W(IXD))
C----   Set up departure coefficients
        call GOGGLE (N, LENPOP(K1), LIMPOP(K1), POPSYM(K1), X(JJTE),
     $               X(JJXNE), X(JJPFT), X(JJCHI), W(IABDL), K1,
     $               XPBL(LLPOPN), XPBL(LLPOPK), XPBL(LLBD), LC, 0,
     $               dummy)
C----   Print
        call ARGIA  (PRINTN, PRINTB, RUNION, LIMPOP(K1), N, X(JJNK),
     $               X(JJXND), X(JJBDI), XPBL(LLPOPK), XPBL(LLPOPN),
     $               XPBL(LLBD))
        call SERF   (NO, LENPOP(K1), N, X(JJHND), X(JJXNE),
     $               XPBL(LLPOPN), XPBL(LLPOPK), KLTE, NAMES(K1),
     $               NAMKNT(K1), LIMPOP(K1), K1, IPSWICH(K1),
     $               XPBL(LLBD), LC, 1, PRINTN, PRINTB)
        if(RUNION) then
          LENPOP(K1) = min(NL,LIMPOP(K1))
        end if
C
C       (Give back W allotment)
        call WGIVE  (W, 'RUBICON')
C     !EJECT
      else
C----   No calculation needed - just set =0
        call OLM  (N, LIMPOP(K1), XPBL(LLPOPK), XPBL(LLPOPN),
     $             XPBL(LLBD))
      end if
C
C---- Checksums
      call PUPPET (ICKSM(K1), N, LIMPOP(K1), XPBL(LLPOPK),
     $             XPBL(LLPOPN), 1, XPBL(LLBD))
C     !END
      call BYE ('RUBICON')
C
      return
      end

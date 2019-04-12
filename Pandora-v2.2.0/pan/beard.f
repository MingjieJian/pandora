      subroutine BEARD
     $(X,W,XPBL,KODE)
C
C     Rudolf Loeser, 1973 Mar 12
C---- Does the initial processing of Hydrogen "populations data".
C     (This is version 2 of BEARD.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XPBL, dummy
      integer IABDL, IN, IRAB, IS, IX1, IXD, JJBDI, JJCHI, JJHND, JJNK,
     $        JJPFT, JJRAB, JJTE, JJXND, JJXNE, JYDRO, KLTE, KODE, LC,
     $        LLBD, LLPOPK, LLPOPN, MLH, MOX, N, NL, NLH, NO
      logical PRINTB, PRINTN, RUNION
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !EJECT
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
      equivalence (LENPOP( 1),NLH )
      equivalence (IUPOP( 1),JYDRO)
C     !DASH
      external  JAM, THRALL, BISHOP, GOGGLE, SERF, RABBIT, ONE1, OLM,
     $          ARGIA, PUPPET, WGIVE, HI, BYE
      intrinsic max, min
C
      dimension X(*), W(*)
C
C               XPBL(Lenpbl)
      dimension XPBL(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1), IX1   ),(IN( 2),IXD   ),(IN( 3),IRAB  ),(IN( 4),IABDL )
C     !EJECT
C
      call HI ('BEARD')
C     !BEG
      if(KODE.eq.0) then
C----   No calculation needed - just set =0
        call OLM    (N, LIMPOP(1), XPBL(LLPOPK), XPBL(LLPOPN),
     $               XPBL(LLBD))
      else
C----   Full solution needed
C       (Get, and allocate, W allotment)
        call JAM    (IN, IS, MOX, 'BEARD')
C
        RUNION = (JYDRO.gt.0)
C----   Set up RAB table
        call RABBIT (RUNION, N, X(JJRAB), W(IRAB))
C----   Set up defaults for NP and HN1
        call THRALL (N, X(JJTE), X(JJCHI), X(JJPFT), X(JJXNE),
     $               X(JJHND), W(IRAB), NLH, XPBL(LLPOPN),
     $               XPBL(LLPOPK), KLTE, W(IX1), W(IXD))
        MLH = max(NLH,1)
C----   Set up remaining levels
        call BISHOP (N, MLH, XPBL(LLPOPN), X(JJTE), X(JJXNE),
     $               XPBL(LLPOPK), LIMPOP(1), W)
C----   Set up departure coefficients
        call ONE1   (W(IABDL), N)
        call GOGGLE (N, MLH, LIMPOP(1), POPSYM(1), X(JJTE), X(JJXNE),
     $               X(JJPFT), X(JJCHI), W(IABDL), 1, XPBL(LLPOPN),
     $               XPBL(LLPOPK), XPBL(LLBD), LC, 0, dummy)
C----   Print
        call ARGIA  (PRINTN, PRINTB, RUNION, LIMPOP(1), N, X(JJNK),
     $               X(JJXND), X(JJBDI), XPBL(LLPOPK), XPBL(LLPOPN),
     $               XPBL(LLBD))
        call SERF   (NO, LENPOP(1), N, X(JJHND), X(JJXNE),
     $               XPBL(LLPOPN), XPBL(LLPOPK), KLTE, NAMES(1),
     $               NAMKNT(1), LIMPOP(1), 1, IPSWICH(1),
     $               XPBL(LLBD), LC, 1, PRINTN, PRINTB)
C
        if(RUNION) then
          NLH = min(NL,LIMPOP(1))
        end if
C
C       (Give back W allotment)
        call WGIVE  (W, 'BEARD')
      end if
C---- Checksums
      call PUPPET      (ICKSM(1), N, LIMPOP(1), XPBL(LLPOPK),
     $                  XPBL(LLPOPN), 1, XPBL(LLBD))
C     !END
      call BYE ('BEARD')
C
      return
      end

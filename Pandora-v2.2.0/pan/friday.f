      subroutine FRIDAY
     $(X,W,BDI,XNE,LUE)
C
C     Rudolf Loeser, 1975 Sep 15
C     Revised RL/SGK Apr  9 2014 
C---- Updates NE, in a Hydrogen populations update run, for GORSE.
C     !DASH
      save
C     !DASH
      real*8 BDI, W, X, XNE
      integer IN, IS, IXPBL, IYPBL, IZHEL, JJHND, JYDRO, LUE, MOX,
     $        JJZME, JJZRN, jummy
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ(236),JJZRN)
C
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
      equivalence (IUPOP( 1),JYDRO)
C     !DASH
C     !EJECT
      external MONDAY, POPIO, CHIRP, SUNDAY, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               BDI(N,NL), XNE(N)
      dimension BDI(*),    XNE(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXPBL ),(IN( 2),IZHEL ),(IN( 3),IYPBL )
C
      call HI ('FRIDAY')
C     !BEG
      if(JYDRO.gt.0) then
C       (Get W allotment)
        call MONDAY (IN, IS, MOX, 'FRIDAY')
C
C       (Initialize populations buffer)
        call POPIO  ('INIT', jummy, W(IXPBL))
C
C----   Get Helium electrons
        call CHIRP  (W, W(IXPBL), W(IZHEL))
C----   Compute total electrons
        call SUNDAY (X, W, LUE, BDI, XNE, X(JJHND), W(IZHEL), X(JJZME),
     $               X(JJZRN), W(IXPBL), W(IYPBL))
C
C       (Give back W allotment)
        call WGIVE  (W, 'FRIDAY')
      end if
C     !END
      call BYE ('FRIDAY')
C
      return
      end

      subroutine PATINA
     $(NPOP,N,NL,XNK,XND,XPBL,ICKSK,ICKSN)
C
C     Rudolf Loeser, 1989 Sep 22
C---- Does final populations updating for BOGACH.
C     !DASH
      save
C     !DASH
      real*8 XND, XNK, XPBL
      integer ICKSK, ICKSN, K, L, LLPOPK, LLPOPN, M, N, NL, NPOP
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
C     !DASH
C     !EJECT
      external  POPIO, MOVE1, WENDY, HI, BYE
      intrinsic min
C
C               XNK(N), XND(N,NL), XPBL(Lenpbl)
      dimension XNK(*), XND(*),    XPBL(*)
C
      call HI ('PATINA')
C     !BEG
      M = min(NL,LIMPOP(NPOP))
C
C---- Update population data block
      L = N*M
      call POPIO ('READ', NPOP, XPBL)
C
      call MOVE1 (XNK, N, XPBL(LLPOPK))
      call MOVE1 (XND, L, XPBL(LLPOPN))
C
      call POPIO ('WRITE', NPOP, XPBL)
C
C---- Update Continuum Recalculation control switches
      K = N*LIMPOP(NPOP)
      call WENDY (XPBL(LLPOPN), 1, K, ICKSN, 'PATINA')
      call WENDY (XPBL(LLPOPK), 1, N, ICKSK, 'PATINA')
C
C     !END
      call BYE ('PATINA')
C
      return
      end

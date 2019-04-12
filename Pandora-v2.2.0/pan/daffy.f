      subroutine DAFFY
     $(Z,TE,XNE,XPBL,COND,LU,W)
C
C     Rudolf Loeser, 1981 Feb 03
C---- Computes conductive flux from the general equations, for VILA.
C     (This is version 2 of DAFFY.)
C     !DASH
      save
C     !DASH
      real*8 COND, TE, W, XNE, XPBL, Z, ZERO
      integer IHE1N1, IHE2N1, IHE2NK, IHN1, IHNK, ILE, ILH, ILHE, ILR,
     $        ILT, IN, IS, LLNPOP, LU, MOX, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LZOQ( 1),LLNPOP)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MAMBA, VERBENA, RATTLER, YUCATAN, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XPBL(Lenpbl), Z(N), TE(N), XNE(N), COND(N)
      dimension XPBL(*),      Z(*), TE(*), XNE(*), COND(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),IHN1  ),(IN( 2),IHNK  ),(IN( 3),IHE1N1),(IN( 4),IHE2N1),
     $(IN( 5),IHE2NK),(IN( 6),ILR   ),(IN( 7),ILE   ),(IN( 8),ILHE  ),
     $(IN( 9),ILH   ),(IN(10),ILT   )
C
      call HI ('DAFFY')
C     !BEG
C     (Get, and allocate, W allotment)
      call MAMBA   (IN, IS, MOX, 'DAFFY')
C
C---- Get H and HE Populations Data.
      call VERBENA (XPBL, W(IHN1), W(IHNK), W(IHE1N1), W(IHE2N1),
     $              W(IHE2NK))
C---- Compute thermal conductivity.
      call RATTLER (TE, W(IHN1), W(IHNK), W(IHE1N1), W(IHE2N1),
     $              W(IHE2NK), XNE, N, LU, W(ILR), W(ILE), W(ILHE),
     $              W(ILH), W(ILT))
C---- Compute conductive flux gradient.
      call YUCATAN (Z, TE, W(ILT), COND, N)
C
C     (Give back W allotment)
      call WGIVE   (W, 'DAFFY')
C     !END
      call BYE ('DAFFY')
C
      return
      end

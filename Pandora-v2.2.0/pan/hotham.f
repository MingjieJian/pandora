      subroutine HOTHAM
     $(W,IW,XLM,XLP,CORE,N,NOPAC,KEMIT,IPOP,CO,ISWA,CB,ISWE,TE,XNC,V,
     $ VXS,XNE,BDHM,XLMXX,XLMDR,XPBL,VEC,TRM,BCKSM,CON,XLYB,KOPAC,EMU,
     $ VEX,H1,CABS,CEMI)
C
C     Rudolf Loeser, 1988 Feb 04
C---- Computes various contributions to absorption and emission
C     requiring "non-LTE populations data".
C     !DASH
      save
C     !DASH
      real*8 BCKSM, BDHM, CABS, CB, CEMI, CO, CON, CORE, EMU, H1, TE,
     $       TRM, V, VEC, VEX, VXS, W, XLM, XLMDR, XLMXX, XLP, XLYB,
     $       XNC, XNE, XPBL
      integer IPOP, ISWA, ISWE, IW, KEMIT, KOPAC, LLBD, LLPOPK, LLPOPN,
     $        N, NOPAC
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
C     !DASH
C     !EJECT
      external POPIO, GLITTER, GLARE, SPARKLE, FUNKE, GLUT, BRAND,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               XPBL(Lenpbl), ISWE(Nopac), BCKSM(NCSBA), TRM(N), TE(N),
      dimension XPBL(*),      ISWE(*),     BCKSM(*),     TRM(*), TE(*),
C
C               KOPAC(Nopac), XNC(N), CO(Nopac,N), CB(Nopac,N), CON(N),
     $          KOPAC(*),     XNC(*), CO(*),       CB(*),       CON(*),
C
C               ISWA(Nopac), CABS(N,Nlin), CEMI(N,Nlin), VEX(N), H1(N),
     $          ISWA(*),     CABS(*),      CEMI(*),      VEX(*), H1(*),
C
C               XLMXX(LLY), XLYB(Lenlyb), V(N), VEC(N), XNE(N), VXS(N),
     $          XLMXX(*),   XLYB(*),      V(*), VEC(*), XNE(*), VXS(*),
C
C               XLMDR(LLY), BDHM(N)
     $          XLMDR(*),   BDHM(*)
C
      call HI ('HOTHAM')
C     !BEG
      if(IPOP.eq.1) then
C----   Hydrogen
        call POPIO   ('ASSURE',  1, XPBL)
        call GLITTER (W, IW, XLM, XLP, N, NOPAC, KEMIT, CO, ISWA, CB,
     $                ISWE, XNE, TE, XNC, BDHM, V, VXS, XLMXX, XLMDR,
     $                XPBL(LLPOPK), XPBL(LLPOPN), XPBL(LLBD), VEC, TRM,
     $                BCKSM, CON, XLYB, KOPAC)
      else if(IPOP.eq.4) then
C----   Helium-I
        call POPIO   ('ASSURE',  4, XPBL)
        call GLARE   (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB,
     $                ISWE, EMU, XNE, TE, V, VEX, H1, XPBL(LLPOPN),
     $                XPBL(LLPOPK), XPBL(LLBD), BCKSM, KOPAC, CABS,
     $                CEMI)
      else if(IPOP.eq.5) then
C----   Helium-II
        call POPIO   ('ASSURE',  5, XPBL)
        call SPARKLE (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB,
     $                ISWE, EMU, XNE, TE, V, VEX, H1, XPBL(LLPOPN),
     $                XPBL(LLBD), BCKSM, KOPAC, CABS, CEMI)
C     !EJECT
      else if(IPOP.eq.11) then
C----   Oxygen-I
        call POPIO   ('ASSURE', 11, XPBL)
        call FUNKE   (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB,
     $                ISWE, EMU, XNE, TE, V, VEX, H1, XPBL(LLPOPN),
     $                XPBL(LLBD), BCKSM, KOPAC, CABS, CEMI)
      else if(IPOP.eq.13) then
C----   Oxygen-II
        call POPIO   ('ASSURE', 13, XPBL)
        call GLUT    (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB,
     $                ISWE, EMU, XNE, TE, V, VEX, H1, XPBL(LLPOPN),
     $                XPBL(LLBD), BCKSM, KOPAC, CABS, CEMI)
      else if(IPOP.eq.14) then
C----   Oxygen-III
        call POPIO   ('ASSURE', 14, XPBL)
        call BRAND   (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB,
     $                ISWE, EMU, XNE, TE, V, VEX, H1, XPBL(LLPOPN),
     $                XPBL(LLBD), BCKSM, KOPAC, CABS, CEMI)
      end if
C     !END
      call BYE ('HOTHAM')
C
      return
      end

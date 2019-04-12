      subroutine GLOAM
     $(W,IW,XLM,XLP,CORE,NOPAC,KEMIT,CO,ISWA,CB,ISWE,XPBL,N,TE,XNC,V,
     $ VXS,B,XNE,BDHM,XLMXX,XLMDR,ORES,OREM,SRES,SREM,H,KRESN,KISLV,
     $ KODE,T1,TR,S1,SR,KUPT,VEC,TRM,BCKSM,CON,XLYB,KOPAC,EMU,VEX,H1,
     $ CABS,CEMI)
C
C     Rudolf Loeser, 1986 Jul 14
C---- Computes bound-free absorption and emission contributions
C     from Population Ions.
C     !DASH
      save
C     !DASH
      real*8 B, BCKSM, BDHM, CABS, CB, CEMI, CO, CON, CORE, EMU, H, H1,
     $       OREM, ORES, S1, SR, SREM, SRES, T1, TE, TR, TRM, V, VEC,
     $       VEX, VXS, W, XLM, XLMDR, XLMXX, XLP, XLYB, XNC, XNE, XPBL
      integer IPOP, ISWA, ISWE, IW, KAPPA, KEMIT, KISLV, KODE, KOPAC,
     $        KRESN, KUPT, LIMD, LLBD, LLPOPN, N, NOPAC
      logical KILROY, UPE, UPO
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
      equivalence (LZOQ( 5),LLBD  )
C     !DASH
C     !EJECT
      external POPIO, POPPA, PABLUM, HOTHAM, MOAN, PINESAP, SPURGE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               H(LIMD), ORES(N), OREM(N), BCKSM(NCSBA), BDHM(N), V(N),
      dimension H(*),    ORES(*), OREM(*), BCKSM(*),     BDHM(*), V(*),
C
C               XLMXX(LLY), XLMDR(LLY), ISWA(Nopac), ISWE(Nopac), B(N),
     $          XLMXX(*),   XLMDR(*),   ISWA(*),     ISWE(*),     B(*),
C
C               KOPAC(Nopac), XLYB(Lenlyb), TR(N), TE(N), H1(N), T1(N),
     $          KOPAC(*),     XLYB(*),      TR(*), TE(*), H1(*), T1(*),
C
C               S1(N), SRES(N), CO(Nopac,N), CB(Nopac,N), XPBL(Lenpbl),
     $          S1(*), SRES(*), CO(*),       CB(*),       XPBL(*),
C
C               SREM(N), CABS(N,Nlin), CEMI(N,Nlin), VEX(N), XNE(N),
     $          SREM(*), CABS(*),      CEMI(*),      VEX(*), XNE(*),
C
C               TRM(N), SR(N), VXS(N), CON(N), VEC(N), XNC(N)
     $          TRM(*), SR(*), VXS(*), CON(*), VEC(*), XNC(*)
C     !EJECT
C
      call HI ('GLOAM')
C     !BEG
      KUPT = 0
C---- Loop over all ions.
      do 100 IPOP = 1,NPOPS
C
        KILROY = .true.
C----   Get absorber number for this ion.
        KAPPA = KAPNO(IPOP)
C
C----   Absorbers
        call PINESAP  (IPOP, BCKSM, KOPAC, XLM, XLP, UPO)
        if(UPO) then
C----     Make sure that Population Data for this ion is
C         present in Buffer.
          call POPIO  ('ASSURE', IPOP, XPBL)
C----     Compute bound-free Opacity components, ORES and OREM
          call POPPA  (KAPPA, XLM, TE, XPBL(LLPOPN), XPBL(LLBD),
     $                 IPOP, H, LIMD, KILROY, N, ORES, OREM, KISLV)
          ISWA(KAPPA) = 1
        end if
C
C----   Emitters
        call SPURGE   (IPOP, BCKSM, ISWA(KAPPA), KEMIT, KOPAC,
     $                 XLM, XLP, UPE)
        if(UPE) then
C----     Make sure of data again.
C         (At present, this step is not strictly necessary, since
C         ISWE(KAPPA) cannot be .gt. 0 when ISWA(KAPPA) is .le. 0)
          call POPIO  ('ASSURE', IPOP, XPBL)
C----     Compute bound-free Emission components, SRES and SREM
          call PABLUM (KAPPA, XLM, B, TE, XPBL(LLBD), IPOP, H, LIMD,
     $                 KILROY, N, SRES, SREM, KISLV)
          ISWE(KAPPA) = 1
        end if
C
C----   Shuffle ORES, OREM, SRES and SREM into proper slots
        call MOAN     (N, NOPAC, ORES, OREM, SRES, SREM, T1, TR, S1,
     $                 SR, UPO, CO, UPE, CB, KAPPA, KRESN, KODE, KUPT)
C
C----   Compute other contributions, if any, requiring these
C       population data
        call HOTHAM   (W, IW, XLM, XLP, CORE, N, NOPAC, KEMIT, IPOP,
     $                 CO, ISWA, CB, ISWE, TE, XNC, V, VXS, XNE, BDHM,
     $                 XLMXX, XLMDR, XPBL, VEC, TRM, BCKSM, CON, XLYB,
     $                 KOPAC, EMU, VEX, H1, CABS, CEMI)
C
  100 continue
C     !END
      call BYE ('GLOAM')
C
      return
      end

      subroutine GLITTER
     $(W,IW,XLM,XLP,N,NOPAC,KEMIT,CO,ISWA,CB,ISWE,XNE,TE,XNC,BDHM,V,VXS,
     $ XLMXX,XLMDR,HNK,HN,BDH,VEC,TRM,BCKSM,CON,XLYB,KOPAC)
C
C     Rudolf Loeser, 1988 Feb 04
C---- Computes absorption and emission depending on H data.
C     (This is version 3 of GLITTER.)
C     !DASH
      save
C     !DASH
      real*8 BCKSM, BDH, BDHM, CB, CO, CON, HN, HNK, TE, TRM, V, VEC,
     $       VXS, W, XLM, XLMDR, XLMXX, XLP, XLYB, XNC, XNE
      integer ISWA, ISWE, IW, KEMIT, KOPAC, N, NOPAC
C     !DASH
      external KOBUK, SEMANG, HI, BYE
C
      dimension W(*), IW(*)
C
C               CO(Nopac,N), ISWA(Nopac), CB(Nopac,N), HNK(N), BDHM(N),
      dimension CO(*),       ISWA(*),     CB(*),       HNK(*), BDHM(*),
C
C               BDH(N,Limdat(1)), XLYB(Lenlyb), XLMXX(LLY), XLMDR(LLY),
     $          BDH(*),           XLYB(*),      XLMXX(*),   XLMDR(*),
C
C               XNC(N), ISWE(Nopac), VXS(N), KOPAC(Nopac), V(N), TE(N),
     $          XNC(*), ISWE(*),     VXS(*), KOPAC(*),     V(*), TE(*),
C
C               HN(N,Limdat(1)), XNE(N), BCKSM(NCSBA), CON(N), TRM(N),
     $          HN(*),           XNE(*), BCKSM(*),     CON(*), TRM(*),
C
C               VEC(N)
     $          VEC(*)
C
      call HI ('GLITTER')
C     !BEG
C---- Absorptions
      call KOBUK    (W, IW, XLM, XLP, N, NOPAC, ISWA, CO, XNE, TE,
     $               XNC, BDHM, V, VXS, XLMXX, XLMDR, HNK, HN, BDH,
     $               VEC, TRM, BCKSM, CON, XLYB, KOPAC)
      if(KEMIT.gt.0) then
C----   Emissions
        call SEMANG (W, IW, XLM, XLP, N, NOPAC, ISWE, CB, BDHM, TE,
     $               HN, BDH, CO, BCKSM, ISWA, XLYB, KOPAC)
      end if
C     !END
      call BYE ('GLITTER')
C
      return
      end

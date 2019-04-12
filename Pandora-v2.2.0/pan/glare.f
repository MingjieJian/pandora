      subroutine GLARE
     $(XLM,XLP,CORE,N,NOPAC,KEMIT,CO,ISWA,CB,ISWE,EMU,XNE,TE,V,VEX,
     $ H1,HE1N,HE1K,HE1BD,BCKSM,KOPAC,CABS,CEMI)
C
C     Rudolf Loeser, 1988 Feb 04
C---- Computes absorption and emission requiring He-I data.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CB, CEMI, CO, CORE, EMU, H1, HE1BD, HE1K,
     $       HE1N, TE, V, VEX, XLM, XLP, XNE
      integer ISWA, ISWE, KEMIT, KOPAC, N, NOPAC
C     !DASH
      external AKHOR, AKOA, BOMOR, HI, BYE
C
C               CO(Nopac,N), ISWA(Nopac), KOPAC(Nopac), XNE(N), VEX(N),
      dimension CO(*),       ISWA(*),     KOPAC(*),     XNE(*), VEX(*),
C
C               HE1N(N,Limd), HE1K(N), BCKSM(NCSBA), CB(Nopac,N), V(N),
     $          HE1N(*),      HE1K(*), BCKSM(*),     CB(*),       V(*),
C
C               CABS(N,Nlin), CEMI(N,Nlin), ISWE(Nopac), HE1BD(N,Limd),
     $          CABS(*),      CEMI(*),      ISWE(*),     HE1BD(*),
C
C               TE(N), H1(N)
     $          TE(*), H1(*)
C
      call HI ('GLARE')
C     !BEG
C---- Absorptions
      call AKHOR  (XLM, XLP, N, NOPAC, ISWA, CO, HE1N, BCKSM, KOPAC)
      if(KEMIT.gt.0) then
C----   Emissions
        call AKOA (XLM, XLP, N, NOPAC, ISWE, CB, HE1N, CO, BCKSM,
     $             ISWA, KOPAC)
      end if
C
C---- He-I background lines
      call BOMOR  (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB,
     $             ISWE, EMU, XNE, TE, V, VEX, H1, HE1N, HE1BD, BCKSM,
     $             KOPAC, CABS, CEMI)
C     !END
      call BYE ('GLARE')
C
      return
      end

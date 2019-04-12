      subroutine BRAND
     $(XLM,XLP,CORE,N,NOPAC,KEMIT,CO,ISWA,CB,ISWE,EMU,XNE,TE,V,VEX,H1,
     $ O3N,O3BD,BCKSM,KOPAC,CABS,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes absorption and emission requiring O-III data.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CB, CEMI, CO, CORE, EMU, H1, O3BD, O3N, TE, V,
     $       VEX, XLM, XLP, XNE
      integer ISWA, ISWE, KEMIT, KOPAC, N, NOPAC
C     !DASH
      external HOLE, HI, BYE
C
C               KOPAC(Nopac), BCKSM(NCSBA), CABS(N,Nlin), CEMI(N,Nlin),
      dimension KOPAC(*),     BCKSM(*),     CABS(*),      CEMI(*),
C
C               V(N), VEX(N), TE(N), XNE(N), O3BD(N,Limd), ISWA(Nopac),
     $          V(*), VEX(*), TE(*), XNE(*), O3BD(*),      ISWA(*),
C
C               ISWE(Nopac), O3N(N,Limd), CB(Nopac,N), CO(Nopac,N),
     $          ISWE(*),     O3N(*),      CB(*),       CO(*),
C
C               H1(N)
     $          H1(*)
C
      call HI ('BRAND')
C     !BEG
C---- O-III background lines
      call HOLE (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB, ISWE,
     $           EMU, XNE, TE, V, VEX, H1, O3N, O3BD, BCKSM, KOPAC,
     $           CABS, CEMI)
C     !END
      call BYE ('BRAND')
C
      return
      end

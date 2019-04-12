      subroutine SPARKLE
     $(XLM,XLP,CORE,N,NOPAC,KEMIT,CO,ISWA,CB,ISWE,EMU,XNE,TE,V,VEX,H1,
     $ HE2N,HE2BD,BCKSM,KOPAC,CABS,CEMI)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Computes absorption and emission requiring He-II data.
C     (This is version 4 of SPARKLE.)
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CB, CEMI, CO, CORE, EMU, H1, HE2BD, HE2N, TE,
     $       V, VEX, XLM, XLP, XNE
      integer ISWA, ISWE, KEMIT, KOPAC, N, NOPAC
C     !DASH
      external ABYSS, HI, BYE
C
C               KOPAC(Nopac), BCKSM(NCSBA), CABS(N,Nlin), CEMI(N,Nlin),
      dimension KOPAC(*),     BCKSM(*),     CABS(*),      CEMI(*),
C
C               V(N), H1(N), TE(N), XNE(N), HE2BD(N,Limd), ISWA(Nopac),
     $          V(*), H1(*), TE(*), XNE(*), HE2BD(*),      ISWA(*),
C
C               ISWE(Nopac), HE2N(N,Limd), CB(Nopac,N), CO(Nopac,N),
     $          ISWE(*),     HE2N(*),      CB(*),       CO(*),
C
C               VEX(N)
     $          VEX(*)
C
      call HI ('SPARKLE')
C     !BEG
C---- He-II background lines
      call ABYSS (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB, ISWE,
     $            EMU, XNE, TE, V, VEX, H1, HE2N, HE2BD, BCKSM, KOPAC,
     $            CABS, CEMI)
C     !END
      call BYE ('SPARKLE')
C
      return
      end

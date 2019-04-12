      subroutine GLUT
     $(XLM,XLP,CORE,N,NOPAC,KEMIT,CO,ISWA,CB,ISWE,EMU,XNE,TE,V,VEX,H1,
     $ O2N,O2BD,BCKSM,KOPAC,CABS,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes absorption and emission requiring O-II data.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CB, CEMI, CO, CORE, EMU, H1, O2BD, O2N, TE, V,
     $       VEX, XLM, XLP, XNE
      integer ISWA, ISWE, KEMIT, KOPAC, N, NOPAC
C     !DASH
      external DITCH, HI, BYE
C
C               KOPAC(Nopac), BCKSM(NCSBA), CABS(N,Nlin), CEMI(N,Nlin),
      dimension KOPAC(*),     BCKSM(*),     CABS(*),      CEMI(*),
C
C               V(N), VEX(N), TE(N), XNE(N), O2BD(N,Limd), ISWA(Nopac),
     $          V(*), VEX(*), TE(*), XNE(*), O2BD(*),      ISWA(*),
C
C               ISWE(Nopac), O2N(N,Limd), CB(Nopac,N), CO(Nopac,N),
     $          ISWE(*),     O2N(*),      CB(*),       CO(*),
C
C               H1(N)
     $          H1(*)
C
      call HI ('GLUT')
C     !BEG
C---- O-II background lines
      call DITCH (XLM, XLP, CORE, N, NOPAC, KEMIT, CO, ISWA, CB, ISWE,
     $            EMU, XNE, TE, V, VEX, H1, O2N, O2BD, BCKSM, KOPAC,
     $            CABS, CEMI)
C     !END
      call BYE ('GLUT')
C
      return
      end

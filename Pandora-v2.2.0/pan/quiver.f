      subroutine QUIVER
     $(NDT,N,DDT,MDTRY1,MDTRY2,XLDT,ADT,ALBDT,XCBL,TDUSTF,TDUSTO,
     $ TDUSTN,OPAC,TAU,S,XJNU,CNXP,PLANK,APD,XDT,RJ,RB,NTRY1,NTRY2,
     $ YFLUX,WTD,Z,HINT,HT,DBH,COEF,G,W,IMG,HK,H,TLTR,TW,VEC,B,YAYB,SA)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Calculates Dust Temperature.
C     !DASH
      save
C     !DASH
      real*8 ADT, ALBDT, APD, B, CNXP, COEF, DBH, DDT, G, H, HINT, HK,
     $       HT, OPAC, PLANK, RB, RJ, S, SA, TAU, TDUSTF, TDUSTN,
     $       TDUSTO, TLTR, TW, VEC, W, WTD, XCBL, XDT, XJNU, XLDT, YAYB,
     $       YFLUX, Z
      integer IMG, MDTRY1, MDTRY2, N, NDT, NTRY1, NTRY2
      logical DUMP
C     !DASH
      external ASPAR, HARPOON, SINEW, ACRAGAS, QUARRY, TULERI, BRABANT,
     $         SHAFT, MASHED, MOVE1, LUPUS, HI, BYE
C
      dimension W(*)
C
C               XDT(NDT), XLDT(NDT), TDUSTO(N), TDUSTN(N), RJ(N), Z(N),
      dimension XDT(*),   XLDT(*),   TDUSTO(*), TDUSTN(*), RJ(*), Z(*),
C
C               PLANK(N,NDT), RB(N), NTRY1(N), NTRY2(N), ADT(N), HK(N),
     $          PLANK(*),     RB(*), NTRY1(*), NTRY2(*), ADT(*), HK(*),
C
C               ALBDT(NDT), XCBL(Miklen), XJNU(N,NDT), APD(NDT), HT(N),
     $          ALBDT(*),   XCBL(*),      XJNU(*),     APD(*),   HT(*),
C
C               OPAC(N,NDT), TAU(N,NDT), S(N,NDT), CNXP(N,NDT), G(N,N),
     $          OPAC(*),     TAU(*),     S(*),     CNXP(*),     G(*),
C
C               TDUSTF(N), HINT(N), DBH(N), COEF(N), H(N,NDT), TW(NDT),
     $          TDUSTF(*), HINT(*), DBH(*), COEF(*), H(*),     TW(*),
C
C               IMG(N), VEC(NDT), B(NDT), YAYB(NDT), SA(NDT,3)
     $          IMG(*), VEC(*),   B(*),   YAYB(*),   SA(*)
C     !EJECT
C
      call HI ('QUIVER')
C     !BEG
C---- Set up dump switch (and header)
      call TULERI   (DDT, DUMP, 'QUIVER')
C
C---- Read continuum data
      call ASPAR    (NDT, XLDT, N, OPAC, TAU, S, XJNU, CNXP, XCBL)
C---- Compute APD
      call HARPOON  (NDT, ADT, ALBDT, APD)
C---- Save previous temperature values (for printing)
      call MOVE1    (TDUSTF, N, TDUSTO)
C
C---- Compute TDUSTN
      call SINEW    (XJNU, APD, XLDT, NDT, N, RJ, TDUSTN, TDUSTO, XDT,
     $               DDT, MDTRY1, PLANK, RB, NTRY1, NTRY2, MDTRY2, TW,
     $               VEC, B, YAYB, DUMP)
C---- Compute TDUSTF
      call BRABANT  (NDT, XLDT, N, Z, TDUSTN, TDUSTF, OPAC, TAU, S,
     $               CNXP, HINT, HT, DBH, COEF, G, XDT, W, IMG, HK, H,
     $               WTD, YFLUX, TLTR, DUMP)
C
C---- Save for iterative summary
      call ACRAGAS  (TDUSTF)
C---- Checksum
      call LUPUS    (TDUSTF)
C
C---- Print things
      call QUARRY   (DDT, MDTRY1, MDTRY2, N, NDT, XDT, APD, RJ, RB,
     $               NTRY1, NTRY2, TDUSTO, TDUSTN, TDUSTF, PLANK, XLDT,
     $               XJNU, H, Z, HINT, DBH, HT, HK, WTD, YFLUX, TLTR)
C---- Plot things
      call SHAFT    (XDT, NDT, XJNU, PLANK, H, N, SA)
C
      if(DUMP) then
        call MASHED ('QUIVER')
      end if
C     !END
      call BYE ('QUIVER')
C
      return
      end

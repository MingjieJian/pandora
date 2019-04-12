      subroutine FINCH
     $(E,NV,VMIN,V,VL,G,FINJ,FINJL,SFINJ,XQ,F,A,NMX,XQMAX,DQMIN,DQMAX,
     $ RACC,CUT,NVD,DUMP)
C
C     Rudolf Loeser, 1984 Jul 06
C---- Computes the injection function for Hydrogen ionization,
C     FINJ(V,E), as a function of V, for a given value of E.
C
C---- DUMP is a switch for dump output;
C     NVD is the index of that value of V for which a dump is wanted
C     (NVD=0 means: no dump, NVD=-1 means: for NV/2).
C     !DASH
      save
C     !DASH
      real*8 A, CUT, DQMAX, DQMIN, E, F, FINJ, FINJL, G, RACC, SFINJ, V,
     $       VL, VMIN, XQ, XQMAX
      integer I, NMX, NV, NVD
      logical DMPV, DUMP
C     !DASH
      external VIREO, SPINEL, ANKER, FIDDLE, HELENA, RUTILE, HI, BYE
C
C               F(NMX), VL(NV), A(NMX), FINJ(NV), FINJL(NV), XQ(NMX),
      dimension F(*),   VL(*),  A(*),   FINJ(*),  FINJL(*),  XQ(*),
C
C               V(NV), G(NV)
     $          V(*),  G(*)
C     !EJECT
C
      call HI ('FINCH')
C     !BEG
      if(DUMP) then
C----   Header
        call SPINEL (E)
      end if
C
C---- Compute V
      call VIREO    (VMIN, E, NV, V, VL)
C---- Loop over all values of V
      do 100 I = 1,NV
C----   Set up dump control
        call ANKER  (DUMP, NV, NVD, I, DMPV)
C----   Compute FINJ, etc.
        call FIDDLE (V(I), E, G(I), FINJ(I), FINJL(I), XQ, F, A, NMX,
     $               XQMAX, DQMIN, DQMAX, RACC, CUT, DMPV)
  100 continue
C
C---- Compute SFINJ, the integral of FINJ over V
      call HELENA   (V, 1, FINJ, 1, A, 1, NV, SFINJ)
C
      if(DUMP) then
        call RUTILE (NV, V, VL, G, FINJ, FINJL, SFINJ)
      end if
C     !END
      call BYE ('FINCH')
C
      return
      end

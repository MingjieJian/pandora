      subroutine DUCKY
     $(XLM,XLP,CORE,OXYGEN2,KOPAC,N,XNE,TE,V,O2N,O2BD,EMU,VEX,H1,
     $ BCKSM,ISWA,CABS)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Supervises calculation of background O-II line opacities.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CORE, EMU, H1, O2BD, O2N, TE, V, VEX, XLM,
     $       XLP, XNE
      integer ISWA, KOPAC, N
      logical OXYGEN2
C     !DASH
      external RUSTY, HI, BYE
C
C               CABS(N,Nlin), KOPAC(Nopac), O2BD(N,Limd), BCKSM(NCSBA),
      dimension CABS(*),      KOPAC(*),     O2BD(*),      BCKSM(*),
C
C               VEX(N), ISWA(Nopac), O2N(N,Limd), TE(N), H1(N), XNE(N),
     $          VEX(*), ISWA(*),     O2N(N,*),    TE(*), H1(*), XNE(*),
C
C               V(N)
     $          V(*)
C
      call HI ('DUCKY')
C     !BEG
      if(KOPAC(44).gt.0) then
C----   O-II lines
        call RUSTY (XLM, CORE, OXYGEN2, N, XNE, TE, V, O2N, O2BD, H1,
     $              VEX, EMU, CABS)
        ISWA(44) = 1
      end if
C     !END
      call BYE ('DUCKY')
C
      return
      end

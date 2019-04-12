      subroutine HALL
     $(XLM,XLP,CORE,OXYGEN3,KOPAC,N,XNE,TE,V,O3N,O3BD,EMU,VEX,H1,
     $ BCKSM,ISWA,CABS)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Supervises calculation of background O-III line opacities.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CORE, EMU, H1, O3BD, O3N, TE, V, VEX, XLM,
     $       XLP, XNE
      integer ISWA, KOPAC, N
      logical OXYGEN3
C     !DASH
      external RAIL, HI, BYE
C
C               CABS(N,Nlin), KOPAC(Nopac), O3BD(N,Limd), BCKSM(NCSBA),
      dimension CABS(*),      KOPAC(*),     O3BD(*),      BCKSM(*),
C
C               VEX(N), ISWA(Nopac), O3N(N,Limd), TE(N), H1(N), XNE(N),
     $          VEX(*), ISWA(*),     O3N(N,*),    TE(*), H1(*), XNE(*),
C
C               V(N)
     $          V(*)
C
      call HI ('HALL')
C     !BEG
      if(KOPAC(45).gt.0) then
C----   O-III lines
        call RAIL (XLM, CORE, OXYGEN3, N, XNE, TE, V, O3N, O3BD, H1,
     $             VEX, EMU, CABS)
        ISWA(45) = 1
      end if
C     !END
      call BYE ('HALL')
C
      return
      end

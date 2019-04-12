      subroutine HELLY
     $(XLM,XLP,CORE,HELIUM2,KOPAC,N,XNE,TE,V,HE2N,HE2BD,EMU,VEX,H1,
     $ BCKSM,ISWA,CABS)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Supervises calculation of background He-II line opacities.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CORE, EMU, H1, HE2BD, HE2N, TE, V, VEX, XLM,
     $       XLP, XNE
      integer ISWA, KOPAC, N
      logical HELIUM2
C     !DASH
      external HEKLA, HI, BYE
C
C               CABS(N,Nlin), ISWA(Nopac), HE2BD(N,Limd), BCKSM(NCSBA),
      dimension CABS(*),      ISWA(*),     HE2BD(*),      BCKSM(*),
C
C               VEX(N), KOPAC(Nopac), HE2N(N,Limd), TE(N), H1(N), V(N),
     $          VEX(*), KOPAC(*),     HE2N(N,*),    TE(*), H1(*), V(*),
C
C               XNE(N)
     $          XNE(*)
C
      call HI ('HELLY')
C     !BEG
      if(KOPAC(33).gt.0) then
C----   He-II lines
        call HEKLA (XLM, CORE, HELIUM2, N, XNE, TE, V, HE2N, HE2BD, H1,
     $              VEX, EMU, CABS)
        ISWA(33) = 1
      end if
C     !END
      call BYE ('HELLY')
C
      return
      end

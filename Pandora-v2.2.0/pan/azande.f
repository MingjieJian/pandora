      subroutine AZANDE
     $(XLM,XLP,CORE,HELIUM1,KOPAC,N,XNE,TE,V,HE1N,HE1BD,EMU,VEX,H1,
     $ BCKSM,ISWA,CABS)
C
C     Rudolf Loeser, 2005 Jun 27
C---- Supervises calculation of background He-I line opacities.
C     (This is version 3 of AZANDE.)
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CORE, EMU, H1, HE1BD, HE1N, TE, V, VEX, XLM,
     $       XLP, XNE
      integer ISWA, KOPAC, N
      logical HELIUM1
C     !DASH
      external NELLY, HI, BYE
C
C               CABS(N,Nlin), ISWA(Nopac), HE1BD(N,Limd), BCKSM(NCSBA),
      dimension CABS(*),      ISWA(*),     HE1BD(*),      BCKSM(*),
C
C               VEX(N), KOPAC(Nopac), HE1N(N,Limd), TE(N), H1(N), V(N),
     $          VEX(*), KOPAC(*),     HE1N(N,*),    TE(*), H1(*), V(*),
C
C               XNE(N)
     $          XNE(*)
C
      call HI ('AZANDE')
C     !BEG
      if(KOPAC(35).gt.0) then
C----   He-I lines
        call NELLY (XLM, CORE, HELIUM1, N, XNE, TE, V, HE1N, HE1BD, H1,
     $              VEX, EMU, CABS)
        ISWA(35) = 1
      end if
C     !END
      call BYE ('AZANDE')
C
      return
      end

      subroutine FLAMME
     $(XLM,XLP,CORE,OXYGEN1,KOPAC,N,XNE,TE,V,ON,OBD,EMU,VEX,H1,BCKSM,
     $ ISWA,CABS)
C
C     Rudolf Loeser, 2004 Mar 31
C---- Supervises calculation of background O-I line opacities.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CORE, EMU, H1, OBD, ON, TE, V, VEX, XLM, XLP,
     $       XNE
      integer ISWA, KOPAC, N
      logical OXYGEN1
C     !DASH
      external BUNNY, HI, BYE
C
C               CABS(N,Nlin), KOPAC(Nopac), OBD(N,Limd), BCKSM(NCSBA),
      dimension CABS(N,*),    KOPAC(*),     OBD(*),      BCKSM(*),
C
C               VEX(N), ISWA(Nopac), ON(N,Limd), TE(N), H1(N), XNE(N),
     $          VEX(*), ISWA(*),     ON(N,*),    TE(*), H1(*), XNE(*),
C
C               V(N)
     $          V(*)
C
      call HI ('FLAMME')
C     !BEG
      if(KOPAC(39).gt.0) then
C----   O-I lines
        call BUNNY (XLM, CORE, OXYGEN1, N, XNE, TE, V, ON, OBD, H1,
     $              VEX, EMU, CABS)
        ISWA(39) = 1
      end if
C     !END
      call BYE ('FLAMME')
C
      return
      end

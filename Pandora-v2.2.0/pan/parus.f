      subroutine PARUS
     $(HND,HK,H1,HEND,HEK,HE1,HE2K,BETA,XNE,VAMB,VBMB,VCMB,VDMB,V1,V2,
     $ V3,VP,VH,VE)
C
C     Rudolf Loeser, 1989 Oct 13
C---- Computes velocities for FALCO.
C     (This is version 2 of PARUS.)
C     !DASH
      save
C     !DASH
      real*8 BETA, H1, HE1, HE2K, HEK, HEND, HK, HND, R1, RH, RK, RS,
     $       TWO, V1, V2, V3, VAMB, VBMB, VCMB, VDMB, VE, VH, VP, XK1,
     $       XK2, XK3, XK4, XK5, XNE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external MUDIR, HI, BYE
C
      call HI ('PARUS')
C     !BEG
      call MUDIR (HND,HK,H1,HEND,HE1,BETA,HE2K,RH,RS,R1,RK,XK1,XK2,XK3,
     $            XK4,XK5)
C
      V1 =         -RH*VBMB+XK1*VCMB+XK3*VDMB
      V2 =         -RH*VBMB-XK2*VCMB+XK3*VDMB
      V3 =         -RH*VBMB-XK2*VCMB-XK4*VDMB
      VP = -R1*VAMB+RS*VBMB
      VH =  RK*VAMB+RS*VBMB
C
      VE = (HK*VP+HEK*V2+TWO*HE2K*V3)/XNE
C     !END
      call BYE ('PARUS')
C
      return
      end

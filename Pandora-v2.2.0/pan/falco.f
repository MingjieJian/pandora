      subroutine FALCO
     $(N,HND,HK,H1,HEND,HEK,HE1,HE2K,BETA,XNE,VAMB,VBMB,VCMB,VDMB,
     $ V1,V2,V3,VP,VH,VE)
C
C     Rudolf Loeser, 1989 Oct 13
C---- Supervises calculation of velocities for diffusion calculations.
C     (This is version 2 of FALCO.)
C     !DASH
      save
C     !DASH
      real*8 BETA, H1, HE1, HE2K, HEK, HEND, HK, HND, V1, V2, V3, VAMB,
     $       VBMB, VCMB, VDMB, VE, VH, VP, XNE
      integer I, N
C     !DASH
      external  PARUS, HI, BYE
C
C               HK(N), H1(N), HEK(N), HE1(N), HE2K(N), BETA(N), XNE(N),
      dimension HK(*), H1(*), HEK(*), HE1(*), HE2K(*), BETA(*), XNE(*),
C
C               VAMB(N), VBMB(N), VCMB(N), HND(N), V1(N), V2(N), VE(N),
     $          VAMB(*), VBMB(*), VCMB(*), HND(*), V1(*), V2(*), VE(*),
C
C               V3(N), VP(N), VH(N), VDMB(N), HEND(N)
     $          V3(*), VP(*), VH(*), VDMB(*), HEND(*)
C
      call HI ('FALCO')
C     !BEG
      do 100 I = 1,N
        call PARUS (HND(I),HK(I),H1(I),HEND(I),HEK(I),HE1(I),HE2K(I),
     $              BETA(I),XNE(I),VAMB(I),VBMB(I),VCMB(I),VDMB(I),
     $              V1(I),V2(I),V3(I),VP(I),VH(I),VE(I))
  100 continue
C     !END
      call BYE ('FALCO')
C
      return
      end

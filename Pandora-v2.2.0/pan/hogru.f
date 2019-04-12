      subroutine HOGRU
     $(HNDV,VNH,NVH,HND,V,N,HNDVL,HNDL,IVNH)
C
C     Rudolf Loeser, 1990 Aug 06
C---- Provides default values of V.
C     !DASH
      save
C     !DASH
      real*8 HND, HNDL, HNDV, HNDVL, V, VNH
      integer IVNH, N, NVH
      logical ZV
C     !DASH
      external NAUGHTD, FABIUS, HI, BYE
C
C               HNDV(NVH), VNH(NVH), HNDVL(NVH), HNDL(N), HND(N), V(N)
      dimension HNDV(*),   VNH(*),   HNDVL(*),   HNDL(*), HND(*), V(*)
C
      call HI ('HOGRU')
C     !BEG
      IVNH = 0
      call NAUGHTD    (V, 1, N, ZV)
C
      if(ZV) then
        if(NVH.gt.0) then
          call FABIUS (HNDV, VNH, NVH, HND, V, N, HNDVL, HNDL)
          IVNH = 1
        end if
      end if
C     !END
      call BYE ('HOGRU')
C
      return
      end

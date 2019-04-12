      subroutine FABIUS
     $(HNDV,VNH,NVH,HND,V,N,HNDVL,HNDL)
C
C     Rudolf Loeser, 1991 Aug 01
C---- Gets values of V (microturbulent velocity) from standard table.
C     !DASH
      save
C     !DASH
      real*8 HND, HNDL, HNDV, HNDVL, V, VNH
      integer I, KONST, N, NVH
C     !DASH
      external DERE, WENDY, HI, BYE
C
C               HNDV(NVH), HNDVL(NVH), VNH(NVH), HND(N), HNDL(N), V(N)
      dimension HNDV(*),   HNDVL(*),   VNH(*),   HND(*), HNDL(*), V(*)
C
      data KONST /1/
C
      call HI ('FABIUS')
C     !BEG
C---- Before interpolating, convert to log(NH)
      do 100 I = 1,NVH
        HNDVL(I) = log(HNDV(I))
  100 continue
      do 101 I = 1,N
        HNDL(I) = log(HND(I))
  101 continue
C---- Parabolic interpolation
      call DERE  (HNDVL, 1, VNH, 1, NVH, HNDL, 1, V, 1, N, KONST)
C
C---- Continuum Recalculation control
      call WENDY (V, 1, N, 2, 'FABIUS')
C     !END
      call BYE ('FABIUS')
C
      return
      end

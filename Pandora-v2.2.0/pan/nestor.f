      subroutine NESTOR
     $(N,DX,FX,GX,TX,EINT,IMAX,DUMP,IA,KODE,LABEL)
C
C     Rudolf Loeser, 1981 Aug 30
C---- Sets up data along a ray, and computes emergent intensity,
C     in spherical coordinates.
C     Also returns TX, the optical depth along the ray.
C     !DASH
      save
C     !DASH
      real*8 DX, EINT, FF, FX, GG, GX, HALF, TX, ZERO
      integer I, IA, IMAX, KODE, N
      logical DUMP
      character LABEL*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
C     !EJECT
      external MENTOR, FURZE, HI, BYE
C
C               DX(N+1), FX(N+1), GX(N+1), TX(N+1)
      dimension DX(*),   FX(*),   GX(*),   TX(*)
C
      call HI ('NESTOR')
C     !BEG
      if(DUMP) then
        call MENTOR (IA, DX, FX, GX, (N+1), LABEL)
      end if
C
      do 100 I = 1,N
        DX(I) = DX(I+1)-DX(I)
        FF = FX(I+1)*FX(I)
        GG = GX(I+1)*GX(I)
        if(FF.gt.ZERO) then
          FX(I) = sqrt(FF)
        else
          FX(I) = HALF*(FX(I+1)+FX(I))
        end if
        if(GG.gt.ZERO) then
          GX(I) = sqrt(GG)
        else
          GX(I) = HALF*(GX(I+1)+GX(I))
        end if
  100 continue
C
      if(DUMP) then
        call MENTOR (0, DX, FX, GX, N, LABEL)
      end if
C
      call FURZE    (DX, FX, GX, N, EINT, TX, IMAX, DUMP, IA, KODE)
C     !END
      call BYE ('NESTOR')
C
      return
      end

      subroutine DOBAR
     $(DIDH,MYX,N,K,LIST,L,YU,YL)
C
C     Rudolf Loeser, 1991 Jul 10
C---- Finds axis limits for the dI/dh plot.
C     !DASH
      save
C     !DASH
      real*8 DIDH, YL, YU
      integer I, J, K, L, LIST, MYX, N
      logical KILROY
C     !DASH
      external  HI, BYE
      intrinsic max, min
C
C               DIDH(N,KM), MYX(KM), LIST(L)
      dimension DIDH(N,*),  MYX(*),  LIST(*)
C
      call HI ('DOBAR')
C     !BEG
      KILROY = .true.
C
      do 100 J = 1,L
C
        if(LIST(J).lt.0) then
          I = MYX(-LIST(J))
          if(KILROY) then
            KILROY = .false.
            YU = DIDH(I,J)
            YL = DIDH(I,J)
          else
            YU = max(YU,DIDH(I,J))
            YL = min(YL,DIDH(I,J))
          end if
        end if
C
  100 continue
C     !END
      call BYE ('DOBAR')
C
      return
      end

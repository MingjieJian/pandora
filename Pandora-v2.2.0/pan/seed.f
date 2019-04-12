      subroutine SEED
     $(IMAGE,AR,N,YSML,YBIG,YM,YU,SYM)
C
C     Rudolf Loeser, 1982 May 05
C---- Enters plot points, for HALYS.
C     (This is version 2 of SEED.)
C     !DASH
      save
C     !DASH
      real*8 AR, X, YBIG, YM, YSML, YU
      integer I, KODE, LINC, N
      character IMAGE*(*), SYM*1
C     !DASH
      external HEEL, HI, BYE
C
C               AR(N)
      dimension AR(*)
C
      call HI ('SEED')
C     !BEG
      LINC = 1
      do 100 I = 1,N
        X = I
        call HEEL (X,AR(I),YSML,YBIG,YM,YU,IMAGE,SYM,KODE,LINC)
        if(KODE.eq.0) then
          LINC = 1
        end if
  100 continue
C     !END
      call BYE ('SEED')
C
      return
      end

      subroutine EPIDOTE
     $(LU,J,NW,N,I1FX,FLX,SFLX,I2FD,FLXD,SFLXD,I3CD,CDF,SCDF,I4CO,COEF,
     $ SCOEF)
C
C     Rudolf Loeser, 1984 Jun 21
C---- Saves data of current wavelength for later printing, for BASHKIR.
C     !DASH
      save
C     !DASH
      real*8 CDF, COEF, FLX, FLXD, SCDF, SCOEF, SFLX, SFLXD
      integer I1FX, I2FD, I3CD, I4CO, J, LU, N, NW
C     !DASH
      external MOVE1, HI, BYE
C
C               SFLX(N,Nmkuse), SFLXD(N,Nmkuse), CDF(N), SCDF(N,Nmkuse),
      dimension SFLX(N,*),      SFLXD(N,*),      CDF(*), SCDF(N,*),
C
C               FLX(N), FLXD(N), COEF(N), SCOEF(N,Nmkuse)
     $          FLX(*), FLXD(*), COEF(*), SCOEF(N,*)
C
      call HI ('EPIDOTE')
C     !BEG
      if(LU.gt.0) then
        if(I1FX.gt.0) then
          call MOVE1 (FLX ,N,SFLX (1,J))
        end if
        if(I2FD.gt.0) then
          call MOVE1 (FLXD,N,SFLXD(1,J))
        end if
        if(I3CD.gt.0) then
          call MOVE1 (CDF ,N,SCDF (1,J))
        end if
        if(I4CO.gt.0) then
          call MOVE1 (COEF,N,SCOEF(1,J))
        end if
      end if
C     !END
      call BYE ('EPIDOTE')
C
      return
      end

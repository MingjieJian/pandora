      subroutine TWINK
     $(NO,WT,NW,N,I1FX,FLX,I2FD,FLXD,I3CD,CDF,I4CO,COEF,YFLUX)
C
C     Rudolf Loeser, 1984 Jun 21
C---- Prints details, for BASHKIR.
C     (This is version 2 of TWINK.)
C     !DASH
      save
C     !DASH
      real*8 CDF, COEF, FLX, FLXD, WT, YFLUX
      integer I1FX, I2FD, I3CD, I4CO, N, NO, NW
C     !DASH
      external ABJECT, LINER, DOUBLER, BENITO, ALMADEN, HI, BYE
C
C               WT(Nmkuse), FLX(N,Nmkuse), FLXD(N,Nmkuse), CDF(N,Nmkuse),
      dimension WT(*),      FLX(*),        FLXD(*),        CDF(*),
C
C               COEF(N,Nmkuse)
     $          COEF(*)
C
      call HI ('TWINK')
C     !BEG
      if(NO.gt.0) then
        call ABJECT    (NO)
        write (NO,100)
  100   format(' ',44X,'Details of Continuum Flux calculation.')
        call LINER     (1,NO)
        call DOUBLER   (NO)
C
        if(I1FX.gt.0) then
          call BENITO  (NO,WT,NW,N,FLX ,' Flux')
        end if
        if(I2FD.gt.0) then
          call BENITO  (NO,WT,NW,N,FLXD,' Flux Derivative')
        end if
        if(I3CD.gt.0) then
          call BENITO  (NO,WT,NW,N,CDF ,' Cumulative Flux Derivative')
        end if
        if(I4CO.gt.0) then
          call ALMADEN (NO,WT,NW,N,COEF,YFLUX)
        end if
      end if
C     !END
      call BYE ('TWINK')
C
      return
      end

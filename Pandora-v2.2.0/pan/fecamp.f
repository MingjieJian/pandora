      subroutine FECAMP
     $(N,J,K,DL,SNU,WVL,VXA,Z,R1N,IRAY,S,EMU,VXAX,VP,DV,DLS,DLR,SNUR,FL)
C
C     Rudolf Loeser, 1983 Sep 02
C---- Shifts SNU, for Shell Rays.
C     The result returns in S.
C     !DASH
      save
C     !DASH
      real*8 DL, DLR, DLS, DLSMAX, DLSMIN, DV, EMU, FL, R1N, S, SNU,
     $       SNUR, VP, VXA, VXAX, WVL, Z
      integer IMAX, IMIN, IRAY, J, K, KRED, N, NRP
      logical DUMP
C     !DASH
      external DRUMMER, DERBY, EPSOM, BARBARA, EUSTACE, WITAN, STIGAND,
     $         RHYTHM, ALE, FYRD, KIRGIZ, HI, BYE
C
C               DL(KM), Z(N), SNU(N,KM), VXA(N), DLS(NRPMX), FL(NRPMX),
      dimension DL(*),  Z(*), SNU(*),    VXA(*), DLS(*),     FL(*),
C
C               EMU(NRPMX), VXAX(NRPMX), VP(NRPMX), DV(NRPMX), DLR(KM),
     $          EMU(*),     VXAX(*),     VP(*),     DV(*),     DLR(*),
C
C               SNUR(NRPMX,KM), S(NRPMX)
     $          SNUR(*),        S(*)
C     !EJECT
C
      call HI ('FECAMP')
C     !BEG
      NRP = 2*IRAY+5
C
C---- Extend V along this ray
      call BARBARA   (VXA, IRAY, VXAX)
C---- Set up MU along ray
      call KIRGIZ    (R1N, Z(N), Z, IRAY, EMU)
C---- Get projected velocity
      call DERBY     (VXAX, EMU, 0, NRP, VP)
C---- Get wavelength shift
      call EPSOM     (WVL, VP, NRP, DV)
C---- Set up shifted Delta-Lambda, and get extrema
      call EUSTACE   (DL(J), DV, DLS, NRP, DLSMIN, DLSMAX)
C---- Find indices of minimum bracketing DL subset
      call WITAN     (DL, K, DLSMIN, DLSMAX, IMIN, IMAX)
C---- Set up reduced tables
      call FYRD      (N, K, DL, SNU, IMIN, IMAX, KRED, DLR, IRAY, NRP,
     $                SNUR)
C---- Now get shifted values
      call ALE       (NRP, KRED, DLR, SNUR, DLS, S, FL)
C
      call RHYTHM    (DUMP)
      if(DUMP) then
        call STIGAND (IRAY, 'FECAMP')
        call DRUMMER (NRP, J, K, DL, WVL, EMU, VXAX, VP, DV, DLS,
     $                DLSMIN, DLSMAX, IMIN, IMAX, KRED, DLR, SNUR, S,
     $                'FECAMP')
      end if
C     !END
      call BYE ('FECAMP')
C
      return
      end

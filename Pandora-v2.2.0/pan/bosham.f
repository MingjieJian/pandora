      subroutine BOSHAM
     $(N,J,K,DL,SNU,WVL,EMOO,VXA,S,EMU,VP,DV,DLS,DLR,SNUR,FL)
C     Rudolf Loeser, 1983 Sep 01
C---- Shifts SNU, in the plane-parallel case.
C     The result resturns in S.
C     !DASH
      save
C     !DASH
      real*8 DL, DLR, DLS, DLSMAX, DLSMIN, DV, EMOO, EMU, FL, S, SNU,
     $       SNUR, VP, VXA, WVL
      integer IMAX, IMIN, J, K, KRED, N
      logical DUMP
C     !DASH
      external SET1, DERBY, EPSOM, EUSTACE, WITAN, DIVES, ALE, MALCOLM,
     $         DRUMMER, RHYTHM, HI, BYE
C
C               DL(KM), SNU(N,KM), VXA(N), S(N), EMU(N), VP(N), DLS(N),
      dimension DL(*),  SNU(*),    VXA(*), S(*), EMU(*), VP(*), DLS(*),
C
C               DV(N), DLR(KM), SNUR(N,KM), FL(N)
     $          DV(*), DLR(*),  SNUR(*),    FL(*)
C
      call HI ('BOSHAM')
C     !BEG
C---- Set up MU table
      call SET1      (EMU, N, EMOO)
C---- Get projected velocity
      call DERBY     (VXA, EMU, 0, N, VP)
C---- Get wavelength shift
      call EPSOM     (WVL, VP, N, DV)
C---- Set up shifted Delta-Lambda, and get extrema
      call EUSTACE   (DL(J), DV, DLS, N, DLSMIN, DLSMAX)
C---- Find indices for minimum bracketing DL subset
      call WITAN     (DL, K, DLSMIN, DLSMAX, IMIN, IMAX)
C---- Set up reduced tables
      call DIVES     (N, K, DL, SNU, IMIN, IMAX, KRED, DLR, SNUR)
C---- Now get shifted values
      call ALE       (N, KRED, DLR, SNUR, DLS, S, FL)
C
      call RHYTHM    (DUMP)
      if(DUMP) then
        call MALCOLM (EMOO, 'BOSHAM')
        call DRUMMER (N, J, K, DL, WVL, EMU, VXA, VP, DV, DLS, DLSMIN,
     $                DLSMAX, IMIN, IMAX, KRED, DLR, SNUR, S, 'BOSHAM')
      end if
C     !END
      call BYE ('BOSHAM')
C
      return
      end

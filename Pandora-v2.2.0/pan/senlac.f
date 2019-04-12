      subroutine SENLAC
     $(N,J,K,DL,SNU,WVL,VXA,Z,FRR,R1N,S,EMU,VP,DV,DLS,DLR,SNUR,FL)
C
C     Rudolf Loeser, 1983 Sep 02
C---- Shifts SNU, for Disk Rays.
C     The result returns in S.
C     !DASH
      save
C     !DASH
      real*8 DL, DLR, DLS, DLSMAX, DLSMIN, DV, EMU, FL, FRR, R1N, S,
     $       SNU, SNUR, VP, VXA, WVL, Z
      integer IMAX, IMIN, J, K, KRED, N
      logical DUMP
C     !DASH
      external KALMYK, DERBY, EPSOM, EUSTACE, WITAN, DIVES, ALE, ULFER,
     $         DRUMMER, RHYTHM, HI, BYE
C
C               DV(N), VP(N), Z(N), FL(N), SNUR(N,KM), SNU(N,KM), S(N),
      dimension DV(*), VP(*), Z(*), FL(*), SNUR(*),    SNU(*),    S(*),
C
C               DLS(N), DLR(KM), DL(KM), VXA(N), EMU(N)
     $          DLS(*), DLR(*),  DL(*),  VXA(*), EMU(*)
C
      call HI ('SENLAC')
C     !BEG
C---- Set up Mu table
      call KALMYK    (R1N, FRR, Z, N, EMU)
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
        call ULFER   (FRR, 'SENLAC')
        call DRUMMER (N, J, K, DL, WVL, EMU, VXA, VP, DV, DLS, DLSMIN,
     $                DLSMAX, IMIN, IMAX, KRED, DLR, SNUR, S, 'SENLAC')
      end if
C     !END
      call BYE ('SENLAC')
C
      return
      end

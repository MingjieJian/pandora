      subroutine HOLLY
     $(TNU,N,TS,M,XM,WN,IB,ZZ,MM,VV)
C
C     Rudolf Loeser, 1968 Jun 04
C---- Computes the specific weight matrix WN (of size N,N) for the
C     optical depth table TNU, by applying a series of transformations
C     (mappings) to the standard weight matrix XM (of size M,M) that
C     pertains to the standard optical depth table TS.
C
C     ZZ, MM, and VV are working storage.
C     !DASH
      save
C     !DASH
      real*8 TNU, TS, VV, WN, XM, ZZ
      integer IB, M, MM, N
C     !DASH
      external WILLOW, ASPEN, POPLAR, UNITSUB, HI, BYE
C
C               ZZ(max[N,M]), MM(max[N,M]), WN(N,N), VV(N,M), XM(M,M),
      dimension ZZ(*),        MM(*),        WN(*),   VV(*),   XM(*),
C
C               TS(M), TNU(N)
     $          TS(*), TNU(*)
C
      call HI ('HOLLY')
C     !BEG
C---- Compute the Z-transformation
      call WILLOW  (TS,M,TNU,N,ZZ,MM)
C---- Form V = Z*XM
      call ASPEN   (N,M,ZZ,MM,XM,VV,IB)
C---- Compute the R-transformation
      call WILLOW  (TNU,N,TS,M,ZZ,MM)
C---- Form WN = V*R
      call POPLAR  (N,M,ZZ,MM,VV,WN)
C---- Modify the first IB diagonal terms of WN
      call UNITSUB (1,IB,WN,N)
C     !END
      call BYE ('HOLLY')
C
      return
      end

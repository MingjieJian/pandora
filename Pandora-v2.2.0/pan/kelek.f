      subroutine KELEK
     $(L,LDL,N,CRD,CVW,CSK,DP,LSAME,COMPUTE)
C
C     Rudolf Loeser, 1990 Oct 04
C---- Decides whether a previous DP-component can be used, or
C     whether a new DP-component must be computed.
C     !DASH
      save
C     !DASH
      real*8 CRD, CSK, CVW, DP
      integer J, L, LDL, LSAME, N
      logical COMPUTE, RD, SK, VW
C     !DASH
      external MOVE1, HI, BYE
C
C               CRD(LDL), CVW(LDL), CSK(LDL), DP(N,LDL)
      dimension CRD(*),   CVW(*),   CSK(*),   DP(N,*)
C
      call HI ('KELEK')
C     !BEG
      COMPUTE = .true.
      LSAME = 0
C
      if(L.gt.1) then
C
        do 100 J = 1,(L-1)
          RD = CRD(J).eq.CRD(L)
          VW = CVW(J).eq.CVW(L)
          SK = CSK(J).eq.CSK(L)
          if(RD.and.VW.and.SK) then
            call MOVE1 (DP(1,J),N,DP(1,L))
            COMPUTE = .false.
            LSAME = J
            goto 101
          end if
  100   continue
C
  101   continue
      end if
C     !END
      call BYE ('KELEK')
C
      return
      end

      subroutine CONTID
     $(INT,INCI,NI,DP,INCD,ND)
C     Rudolf Loeser, 1984 Aug 22
C---- Given the array INT of length NI, and the array DP of length ND.
C     DP is type double precision, INT is type integer.
C     This routine converts and copies the contents of INT into DP as
C     follows:
C                  INT(1+0*INCI) into DP(1+0*INCD),
C                  INT(1+1*INCI) into DP(1+1*INCD),
C                  INT(1+2*INCI) into DP(1+2*INCD),
C                               etc.
C
C---- If NI.gt.ND, then only the first ND elements of INT will be
C     copied into DP. If ND.gt.NI, then the last (ND-NI) elements
C     of DP will not be changed by this routine.
C     !DASH
      save
C     !DASH
      real*8 DP
      integer I, INCD, INCI, IND, INI, INT, M, ND, NI
C     !DASH
      intrinsic min
C
      dimension DP(*),INT(*)
C
C     !BEG
      M = min(NI,ND)
      if(M.gt.0) then
        INI = 1-INCI
        IND = 1-INCD
        do 100 I = 1,M
          INI = INI+INCI
          IND = IND+INCD
          DP(IND) = INT(INI)
  100   continue
      end if
C     !END
C
      return
      end

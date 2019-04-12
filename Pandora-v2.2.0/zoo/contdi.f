      subroutine CONTDI
     $(DP,INCD,ND,INT,INCI,NI)
C     Rudolf Loeser, 1984 Aug 22
C---- Given the array DP of length ND, and the array INT of length NI.
C     DP is type double precision, INT is type integer.
C     This routine converts and copies the contents of DP into INT as
C     follows:
C                  DP(1+0*INCD) into INT(1+0*INCI),
C                  DP(1+1*INCD) into INT(1+1*INCI),
C                  DP(1+2*INCD) into INT(1+2*INCI),
C                               etc.
C
C---- If ND.gt.NI, then only the first NI elements of DP will be copied
C     into INT. If NI.gt.ND, then the last (NI-ND) elements of INT will
C     not be changed by this routine.
C     !DASH
      save
C     !DASH
      real*8 DP
      integer I, INCD, INCI, IND, INI, INT, M, ND, NI
C     !DASH
      intrinsic min
C
      dimension DP(*), INT(*)
C
C     !BEG
      M = min(ND,NI)
      if(M.gt.0) then
        IND = 1-INCD
        INI = 1-INCI
        do 100 I = 1,M
          IND = IND+INCD
          INI = INI+INCI
          INT(INI) = DP(IND)
  100   continue
      end if
C     !END
C
      return
      end

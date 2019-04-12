      subroutine ORIZABA
     $(JN,MTR,TS,M,TAUIJ,N,MARK,CTAU,NPNT,IPNT,IWRK)
C
C     Rudolf Loeser, 1982 May 11
C---- Assembles and sorts collated TAU scales, for PICTURE.
C     !DASH
      save
C     !DASH
      real*8 CTAU, TAUIJ, TS
      integer IPNT, IWRK, J, JN, M, MARK, MTR, N, NMO, NPNT
C     !DASH
      external MOVE1, SETI, SORT, ORDERI, HI, BYE
C
C               NPNT = M+(N-1)*min(NT,LIMSCL), with LIMSCL in ALVIN
C
C               TS(M), TAUIJ(N,NT), MARK(NPNT), CTAU(NPNT), IPNT(NPNT),
      dimension TS(*), TAUIJ(N,*),  MARK(*),    CTAU(*),    IPNT(*),
C
C               IWRK(NPNT), JN(MTR)
     $          IWRK(*),    JN(*)
C
      call HI ('ORIZABA')
C     !BEG
      call MOVE1   (TS, M, CTAU)
      call SETI    (MARK, 1, M, 0)
      NPNT = M
C
      NMO = N-1
      do 100 J = 1,MTR
        call MOVE1 (TAUIJ(2,JN(J)), NMO, CTAU(NPNT+1))
        call SETI  (MARK(NPNT+1), 1, NMO,J)
        NPNT = NPNT+NMO
  100 continue
C
      call SORT    (CTAU, NPNT, IPNT, 'Collated TAU scales')
      call ORDERI  (MARK, IPNT, NPNT, IWRK)
C     !END
      call BYE ('ORIZABA')
C
      return
      end

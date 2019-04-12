      subroutine PICTURE
     $(NO,TS,M,TAUIJ,N,NT,CTAU,MARK,IPNT,IVEC)
C
C     Rudolf Loeser, 1982 May 10
C---- Prints a collated set of TAU scales.
C     (This is version 2 of PICTURE.)
C     !DASH
      save
C     !DASH
      real*8 CTAU, TAUIJ, TS
      integer IPNT, IVEC, JL, JN, JU, M, MARK, MTR, N, NO, NPNT, NT
C     !COM
C---- ALVIN       as of 1995 Aug 08
      integer     LIMSCL
      parameter   (LIMSCL=11)
C     The number of TAU-scales to be printed fancily.
C     (Used in PICTURE, etc.)
C     .
C     !DASH
      external HOOK, CHOLULA, ORIZABA, ATALAYA, HI, BYE
C
C               MPNT = M + min(NT,LIMSCL) * (N-1)
C
C               TS(M), TAUIJ(N,NT), CTAU(MPNT), IPNT(MPNT), IVEC(MPNT),
      dimension TS(*), TAUIJ(*),    CTAU(*),    IPNT(*),    IVEC(*),
C
C               MARK(MPNT)
     $          MARK(*)
C
      dimension JU(LIMSCL), JL(LIMSCL), JN(LIMSCL)
C
      call HI ('PICTURE')
C     !BEG
C---- Get transition indices
      call HOOK    (NT,JU,JL,JN,MTR,LIMSCL)
C---- Print heading
      call CHOLULA (JU,JL,MTR,NO)
C---- Get collated scales
      call ORIZABA (JN,MTR,TS,M,TAUIJ,N,MARK,CTAU,NPNT,IPNT,IVEC)
C---- Print scales
      call ATALAYA (NO,MARK,CTAU,NPNT)
C     !END
      call BYE ('PICTURE')
C
      return
      end

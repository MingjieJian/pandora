      subroutine ERSILIA
     $(N,XKPC,BBC,SIG,FMULT,CAPPA,BHSNMS,SIGSTR)
C
C     Rudolf Loeser, 1981 Jul 31
C---- Makes XKPC, SIG and BBC, for YEAST.
C     (This is version 2 of ERSILIA.)
C     !DASH
      save
C     !DASH
      real*8 BBC, BHSNMS, CAPPA, FMULT, SIG, SIGSTR, XKPC
      integer N
C     !DASH
      external MOVE1, CONMUL, ARRDIV, HI, BYE
C
C               XKPC(N), BBC(N), SIG(N), CAPPA(N), BHSNMS(N), SIGSTR(N)
      dimension XKPC(*), BBC(*), SIG(*), CAPPA(*), BHSNMS(*), SIGSTR(*)
C
      call HI ('ERSILIA')
C     !BEG
      call MOVE1  (CAPPA, N, XKPC)
      call CONMUL (FMULT, XKPC, N)
C
      call MOVE1  (SIGSTR, N, SIG)
      call CONMUL (FMULT, SIG,  N)
C
      call ARRDIV (BHSNMS, CAPPA, BBC, N)
C     !END
      call BYE ('ERSILIA')
C
      return
      end

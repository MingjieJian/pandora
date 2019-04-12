      subroutine PONGO
     $(N,NL,KOLEV,KDGV,NSL,GMI,XND,RLI,CKI,RKI,CQS,GNVL,RS,PKL,PLK1,
     $ SPKL,EP1,EP2)
C
C     Rudolf Loeser, 1990 Apr 12
C---- Computes Lyman epsilons by the "COMPLEX-UPPER-like" method.
C     !DASH
      save
C     !DASH
      real*8 CKI, CQS, EP1, EP2, GMI, GNVL, PKL, PLK1, RKI, RLI, RS,
     $       SPKL, XND
      integer KDGV, KOLEV, M, N, NL, NSL
      logical DUMP, KILROY
C     !DASH
      external BRAZIL, ALPAN, DEER, GOLOT, MASHED, HI, BYE
C
C               GMI(N,NSL), GNVL(N,NL), RLI(N,NSL), CKI(N,NL), PLK1(N),
      dimension GMI(*),     GNVL(*),    RLI(*),     CKI(*),    PLK1(*),
C
C               RKI(N,NSL), XND(N,NL), SPKL(N), EP1(N), EP2(N), PKL(N),
     $          RKI(*),     XND(*),    SPKL(*), EP1(*), EP2(*), PKL(*),
C
C               RS(N), CQS(N,NSL)
     $          RS(*), CQS(*)
C
      call HI ('PONGO')
C     !BEG
      M = KOLEV
C
C---- Print heading for optional printout
      call BRAZIL   (DUMP, KILROY, M, 'Complex-Upper', KDGV, 'PONGO')
C---- Compute levels-sums
      call ALPAN    (N, NL, M, PKL, PLK1, GMI, XND, CQS, CKI, RKI, GNVL)
C---- Supplementary levels term
      call DEER     (N, NL, NSL, KOLEV, GMI, RLI, RKI, SPKL)
C---- Compute and print epsilons
      call GOLOT    (N, NL, M, EP1, EP2, RS, CKI, PKL, SPKL, GNVL, PLK1,
     $               DUMP)
C
      if(.not.KILROY) then
        call MASHED ('PONGO')
      end if
C     !END
      call BYE ('PONGO')
C
      return
      end

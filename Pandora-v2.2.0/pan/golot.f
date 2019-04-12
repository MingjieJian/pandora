      subroutine GOLOT
     $(N,NL,M,EP1,EP2,RS,CKI,PKL,SPKL,GVL,PLK1,DUMP)
C
C     Rudolf Loeser, 1990 Apr 12
C---- Computes Lyman epsilons by the "COMPLEX-UPPER-like" method.
C     (This is version 2 of GOLOT.)
C     !DASH
      save
C     !DASH
      real*8 CKI, EP1, EP2, GVL, PKL, PLK1, RS, SPKL
      integer M, N, NL
      logical DUMP
C     !DASH
      external ARRADD, ARRDIV, KELSO, HI, BYE
C
C               EP1(N), EP2(N), GVL(N,NL), CKI(N,NL), PKL(N), SPKL(N),
      dimension EP1(*), EP2(*), GVL(N,*),  CKI(N,*),  PKL(*), SPKL(*),
C
C               PLK1(N), RS(N)
     $          PLK1(*), RS(*)
C
      call HI ('GOLOT')
C     !BEG
      call ARRADD (CKI(1,M), PKL     , EP1, N)
      call ARRADD (EP1     , SPKL    , EP1, N)
      call ARRDIV (EP1     , RS      , EP1, N)
      call ARRADD (CKI(1,M), GVL(1,M), EP2, N)
      call ARRADD (EP2     , PLK1    , EP2, N)
      call ARRDIV (EP2     , RS      , EP2, N)
C
      call KELSO  (N, NL, M, EP1, EP2, RS, PKL, SPKL, GVL, PLK1, DUMP)
C     !END
      call BYE ('GOLOT')
C
      return
      end

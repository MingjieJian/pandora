      subroutine GAMA
     $(N,NL,LIMP,KODE,D,X,TE,IPOP,POPK,POP)
C
C     Rudolf Loeser, 1978 Sep 27
C---- Computes a set of LTE populations.
C     !DASH
      save
C     !DASH
      real*8 D, POP, POPK, TE, X
      integer IPOP, KODE, LIMP, N, NL
C     !DASH
      external ARRMUL, MOVE1, GAGGLE, HI, BYE
C
C               POPK(N), D(N), X(N), TE(N), POP(N,LIMP)
      dimension POPK(*), D(*), X(*), TE(*), POP(*)
C
      call HI ('GAMA')
C     !BEG
      if(KODE.le.0) then
C----   Compute ionized population
        call ARRMUL (D, X, POPK, N)
      end if
C
      if(NL.le.0) then
C----   Set up ground state population
        call MOVE1  (D, N, POP)
      end if
C
      if(NL.lt.LIMP) then
C----   Compute upper levels populations
        call GAGGLE (N, NL, TE, IPOP, POP, LIMP)
      end if
C     !END
      call BYE ('GAMA')
C
      return
      end

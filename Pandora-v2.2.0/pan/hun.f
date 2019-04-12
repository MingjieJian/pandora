      subroutine HUN
     $(N,NLB,TE,XLM,BATAL)
C
C     Rudolf Loeser, 1982 Feb 19
C---- Computes stimulated emission factors for each level, population
C     update case.
C     (See also GOTH.)
C     !DASH
      save
C     !DASH
      real*8 BATAL, HNUKT, TE, XLM
      integer I, J, N, NLB
C     !DASH
      external PROD, HI, BYE
C
C               TE(N), XLM(NLB), BATAL(N,NLB)
      dimension TE(*), XLM(*),   BATAL(N,*)
C
      call HI ('HUN')
C     !BEG
      do 101 J = 1,NLB
        do 100 I = 1,N
          call PROD (TE(I), XLM(J), 2, HNUKT, BATAL(I,J))
  100   continue
  101 continue
C     !END
      call BYE ('HUN')
C
      return
      end

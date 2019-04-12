      subroutine CHEQUE
     $(BDIJ,BRAS,N,NL,NLM,KIJ,CEK,BA,BB,BC)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Computes consistency CHECKS.
C     (This is version 4 of CHEQUE.)
C     !DASH
      save
C     !DASH
      real*8 BA, BB, BC, BDIJ, BRAS, CEK
      integer I, J, KIJ, MA, MB, MC, N, NL, NLM
C     !DASH
      external ENDORS, MOVE1, ONE1, DIVIDE, HI, BYE
C
C               CEK(N,NLM), BRAS(N,NT), BDIJ(N,NL), BA(N), KIJ(NL,NL),
      dimension CEK(N,*),   BRAS(*),    BDIJ(*),    BA(*), KIJ(*),
C
C               BC(N), BB(N)
     $          BC(*), BB(*)
C
      call HI ('CHEQUE')
C     !BEG
      if(NLM.gt.0) then
C----   Initialize
        call ENDORS       (2  , 1  , NL, 1, N, N, KIJ, BDIJ, BRAS,
     $                     BB, MB)
C----   Compute new CHECKs
        do 101 J = 1,NLM
C----     Set up components of current set
          call MOVE1      (BB, N, BA)
          MA = MB
          call ENDORS     (J+2, J+1, NL, 1, N, N, KIJ, BDIJ, BRAS,
     $                     BB, MB)
          call ENDORS     (J+2, J  , NL, 1, N, N, KIJ, BDIJ, BRAS,
     $                     BC, MC)
C----     Combine components if necessary
          if((MA+MB+MC).eq.0) then
            call ONE1     (CEK(1,J), N)
          else
            do 100 I = 1,N
              call DIVIDE ((BA(I)*BB(I)), BC(I), CEK(I,J))
  100       continue
          end if
  101   continue
      end if
C     !END
      call BYE ('CHEQUE')
C
      return
      end

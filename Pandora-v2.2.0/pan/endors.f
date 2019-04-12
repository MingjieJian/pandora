      subroutine ENDORS
     $(J,K,NL,IS,IE,N,KIJ,BDIJ,BRAS,B,MB)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Gets the b-ratios for transition (J,K).
C     BDIJ is the general b-ratios set (i.e. b(L)/b(1));
C     BRAS is the set of b-ratios from PERSEUS (i.e. for
C     radiative transitions).
C---- Upon return, B will be the desired set.
C     MB=1 means: B came from BRAS;
C     MB=0 means: B came from BDIJ.
C     (This is version 2 of ENDORS.)
C     !DASH
      save
C     !DASH
      real*8 B, BDIJ, BRAS
      integer I, IE, IN, IS, J, K, KIJ, MB, N, NL
C     !DASH
      external INTRANS, MOVE1, BRAT, HI, BYE
C
C               B(N), BRAS(N,NT), KIJ(NL,NL), BDIJ(N,NL)
      dimension B(*), BRAS(N,*),  KIJ(NL,*),  BDIJ(*)
C
      call HI ('ENDORS')
C     !BEG
      if(KIJ(J,K).eq.1) then
        call INTRANS (J, K, 'ENDORS', IN)
        call MOVE1   (BRAS(IS,IN), (IE-IS+1), B(IS))
        MB = 1
      else
        do 100 I = IS,IE
          call BRAT  (I, J, K, BDIJ, B(I))
  100   continue
        MB = 0
      end if
C     !END
      call BYE ('ENDORS')
C
      return
      end

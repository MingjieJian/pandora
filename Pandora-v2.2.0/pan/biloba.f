      subroutine BILOBA
     $(TNU,N,FIN,IL,TSMLL,IS,TNP,K)
C
C     Rudolf Loeser, 1989 Nov 02
C---- Gets the selection indices and sets up the reduced-TAU table,
C     for the RT weight matrix method.
C     (This is version 3 of BILOBA.)
C     !DASH
      save
C     !DASH
      real*8 TNP, TNU, TSMLL
      integer IL, IS, K, N
      logical FIN
C     !DASH
      external  NOTMORE, MOVE1, HI, BYE
C
C               TNU(N), TNP(N)
      dimension TNU(*), TNP(*)
C
      call HI ('BILOBA')
C     !BEG
C---- Large-tau index, IL
C     (No large-tau-elimination allowed)
      IL = N+1
C
C---- Small-tau index, IS
      call NOTMORE (TNU, N, TSMLL, IS)
C
      K = IL-IS
C
      if(FIN.and.(K.lt.4)) then
C----   Assure a minimal reduced set
        IS = N-3
        K  = 4
      end if
C
C---- Reduced-TAU table, TNP
      TNP(1) = TNU(1)
      call MOVE1   (TNU(IS+1), (K-1), TNP(2))
C     !END
      call BYE ('BILOBA')
C
      return
      end

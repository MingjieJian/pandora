      subroutine GINGKO
     $(TNU,N,TLRGE,IL,TSMLL,IS,TNP,K)
C
C     Rudolf Loeser, 1989 Nov 02
C---- Gets the selection indices and sets up the reduced-TAU table,
C     for the QR methods.
C     (This is version 2 of GINGKO.)
C     !DASH
      save
C     !DASH
      real*8 TLRGE, TNP, TNU, TSMLL, ZERO
      integer IL, IS, K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external NOTLESS, NOTMORE, MOVE1, HI, BYE
C
C               TNU(N), TNP(N)
      dimension TNU(*), TNP(*)
C
      call HI ('GINGKO')
C     !BEG
C---- Large-tau index, IL
      call NOTLESS   (TNU, N, TLRGE, IL)
      if(IL.eq.0) then
        IL = N+1
      end if
C
      if(IL.le.3) then
C----   Useless to go on
        K = 0
C
      else
C----   Small-tau index, IS
        call NOTMORE (TNU, N, TSMLL, IS)
C----   Set up reduced-TAU table, TNP
        K = IL-IS
        TNP(1) = ZERO
        call MOVE1   (TNU(IS+1), (K-1), TNP(2))
      end if
C     !END
      call BYE ('GINGKO')
C
      return
      end

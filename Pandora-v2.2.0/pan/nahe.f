      subroutine NAHE
     $(N,FOLD,FNEW,FINL,EXP)
C
C     Rudolf Loeser, 2001 Jun 13
C---- Special weighting routine.
C     (This is version 3 of NAHE.)
C     !DASH
      save
C     !DASH
      real*8 EXP, FINL, FNEW, FOLD, ONE, W, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOVE1, HI, BYE
C
C               FOLD(N), FNEW(N), FINL(N)
      dimension FOLD(*), FNEW(*), FINL(*)
C
      call HI ('NAHE')
C     !BEG
      if(EXP.eq.ZERO) then
        call MOVE1 (FNEW, N, FINL)
      else
        do 100 I = 1,N
          W       = FOLD(I)**EXP
          FINL(I) = (ONE-W)*FOLD(I)+W*FNEW(I)
  100   continue
      end if
C     !END
      call BYE ('NAHE')
C
      return
      end

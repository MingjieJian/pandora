      subroutine OLARBAR
     $(DIDH,MYX,DMAX,RUN,N,OK)
C
C     Rudolf Loeser, 1991 aug 09
C---- Normalizes a run of dI/dh, for OSTUNI.
C     !DASH
      save
C     !DASH
      real*8 DIDH, DMAX, RUN, ZERO
      integer MYX, N
      logical OK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOVE1, CONDIV, HI, BYE
C
C               DIDH(N), RUN(N)
      dimension DIDH(*), RUN(*)
C
      call HI ('OLARBAR')
C     !BEG
      call MOVE1    (DIDH,N,RUN)
      DMAX = DIDH(MYX)
      OK   = DMAX.ne.ZERO
      if(OK) then
        call CONDIV (DMAX,RUN,N)
      end if
C     !END
      call BYE ('OLARBAR')
C
      return
      end

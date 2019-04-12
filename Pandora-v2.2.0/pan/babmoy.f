      subroutine BABMOY
     $(F,N,TYPE,DOLOG)
C
C     Rudolf Loeser, 1998 Oct 16
C---- Sets up DOLOG, for SMOOTH.
C     !DASH
      save
C     !DASH
      real*8 F, ZERO
      integer KEQ, N
      logical DOLOG
      character TYPE*3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RANGED, HI, BYE
C
C               F(N)
      dimension F(*)
C
      call HI ('BABMOY')
C     !BEG
      DOLOG = TYPE.eq.'log'
      if(DOLOG) then
        call RANGED (F, 1, N, ZERO, ZERO, KEQ)
        DOLOG = KEQ.le.0
      end if
C     !END
      call BYE ('BABMOY')
C
      return
      end

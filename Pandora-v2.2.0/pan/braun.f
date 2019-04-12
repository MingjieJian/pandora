      subroutine BRAUN
     $(NSL,NPQ,P)
C
C     Rudolf Loeser, 1992 Mar 27
C---- Sets up default values of statistical weights for Hydrogen.
C     (This is version 3 of BRAUN.)
C     !DASH
      save
C     !DASH
      real*8 P, ZERO
      integer J, JDKNT, JDPSW, NPQ, NSL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(18),JDPSW)
      equivalence (MEST(21),JDKNT)
C     !DASH
      external RUBAN, HI, BYE
C
C               NPQ(NSL), P(NSL)
      dimension NPQ(*),   P(*)
C
      call HI ('BRAUN')
C     !BEG
      do 100 J = 1,NSL
C
        if(P(J).eq.ZERO) then
          call RUBAN (NPQ(J), P(J))
          JDPSW = JDPSW+1
        else
          JDKNT = JDKNT+1
        end if
C
  100 continue
C     !END
      call BYE ('BRAUN')
C
      return
      end

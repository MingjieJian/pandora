      subroutine HICKORY
     $(M,J,DT,S,E)
C
C     Rudolf Loeser, 1981 Mar 31
C---- Obtains ES(M,DT(J)).
C     (This is version 3 of HICKORY.)
C     !DASH
      save
C     !DASH
      real*8 DT, E, S, ZERO
      integer J, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MARDUK, HI, BYE
C
C               DT(2*N), S(2*N)
      dimension DT(*),   S(*)
C
      call HI ('HICKORY')
C     !BEG
      if(S(J).eq.ZERO) then
        call MARDUK (DT(J),M,S(J))
      end if
C
      E = S(J)
C     !END
      call BYE ('HICKORY')
C
      return
      end

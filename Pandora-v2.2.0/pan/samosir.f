      subroutine SAMOSIR
     $(ATOTAL,ACOMP,W,N)
C
C     Rudolf Loeser, 1989 Jul 14
C---- Adds a weighted component array to a total array,
C     term-by-term.
C     !DASH
      save
C     !DASH
      real*8 ACOMP, ATOTAL, ONE, W
      integer N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ARRADD, ARRINC, HI, BYE
C
C               ATOTAL(N,N), ACOMP(N,N)
      dimension ATOTAL(*),   ACOMP(*)
C
      call HI ('SAMOSIR')
C     !BEG
      if(W.eq.ONE) then
        call ARRADD (ACOMP,ATOTAL,ATOTAL,(N**2))
      else
        call ARRINC (ACOMP,W     ,ATOTAL,(N**2))
      end if
C     !END
      call BYE ('SAMOSIR')
C
      return
      end

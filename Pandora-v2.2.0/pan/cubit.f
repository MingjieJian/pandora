      subroutine CUBIT
     $(KUP,LLO,IU,IL,NL,SAIJ,YBRIJ,X,IX,Q)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes "Q" for the VAMOS method.
C     !DASH
      save
C     !DASH
      real*8 Q, SAIJ, X, YBRIJ, ZERO
      integer IL, IU, IX, JJAIJ, KUP, LLO, NL
      logical SINGLE
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external QUORUM, LALA, HI, BYE
C
      dimension X(*), IX(*)
C
C               SAIJ(NL,NL), YBRIJ(NL,NL)
      dimension SAIJ(NL,*),  YBRIJ(*)
C
      call HI ('CUBIT')
C     !BEG
      if((KUP.eq.IU).and.(LLO.eq.IL)) then
        Q = ZERO
      else
        call QUORUM (KUP, LLO, IU, IL, IX, SINGLE)
        if(SINGLE) then
          call LALA (KUP, LLO, X(JJAIJ), YBRIJ, X, Q)
        else
          Q = -SAIJ(KUP,LLO)
        end if
      end if
C     !END
      call BYE ('CUBIT')
C
      return
      end

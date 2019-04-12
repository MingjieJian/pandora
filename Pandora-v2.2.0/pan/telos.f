      subroutine TELOS
     $(J,I,IU,IL,NL,YBRIJ,YIJ,X,IX,T)
C
C     Rudolf Loeser, 2003 Nov 25
C---- Computes "T" for the VAMOS method.
C     !DASH
      save
C     !DASH
      real*8 T, X, YBRIJ, YIJ, ZERO
      integer I, IL, IU, IX, J, JJAIJ, JJP, NL
      logical SINGLE
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 27),JJP  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external QUORUM, LAHDIDA, HI, BYE
C
      dimension X(*), IX(*)
C
C               YBRIJ(NL,NL), YIJ(NL,NL)
      dimension YBRIJ(*),     YIJ(NL,*)
C
      call HI ('TELOS')
C     !BEG
      if((J.eq.IU).and.(I.eq.IL)) then
        T = ZERO
      else
C
        call QUORUM    (J, I, IU, IL, IX, SINGLE)
        if(SINGLE) then
          call LAHDIDA (J, I, NL, X, YBRIJ, X(JJAIJ), X(JJP), T)
        else
          T = YIJ(J,I)
        end if
      end if
C     !END
      call BYE ('TELOS')
C
      return
      end

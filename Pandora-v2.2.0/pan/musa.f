      subroutine MUSA
     $(ITAU,KUP,JLO,KRJ,IU,IL,YBRIJ,AIJ,KIJ,LIJ,P,ALF,YBPIJ,XNU,Z,
     $ XINK,FINK,TERM)
C
C     Rudolf Loeser, 1978 Dec 01
C---- Calculates the "Term added to upward C".
C                               > Not for VAMOS!
C
C     See also LUSA.
C     !DASH
      save
C     !DASH
      real*8 AIJ, ALF, FAC, FINK, P, PRAT, TERM, XINK, XNU, YBPIJ,
     $       YBRIJ, Z, ZERO
      integer IL, ITAU, IU, IUL, JLO, JUL, K, KIJ, KRJ, KUP, LIJ, N, NL
      logical SINGLE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXUL, INTRANS, QUINOA, DIVIDE, PEN, HI, BYE
C
C               AIJ(NL,NL), KIJ(NL,NL), LIJ(MUL), P(NSL), YBPIJ(N,NT),
      dimension AIJ(NL,*),  KIJ(NL,*),  LIJ(*),   P(*),   YBPIJ(N,*),
C
C               YBRIJ(N,NT), ALF(MUL), XNU(NSL), XINK(INK), FINK(INK),
     $          YBRIJ(N,*),  ALF(*),   XNU(*),   XINK(*),   FINK(*),
C
C               Z(N)
     $          Z(*)
C     !EJECT
C
      call HI ('MUSA')
C     !BEG
      K = KIJ(KUP,JLO)
      if((K.ge.1).and.(K.le.4)) then
C
        call INDXUL     (KUP, JLO, IUL)
        call INTRANS    (KUP, JLO, 'MUSA', JUL)
        call QUINOA     (KUP, JLO, IU, IL, KRJ, LIJ(IUL), SINGLE)
        call DIVIDE     (P(KUP), P(JLO), PRAT)
C
        if(K.eq.1) then
C----     For radiative transitions
          if(SINGLE) then
            call DIVIDE (YBRIJ(ITAU,JUL), ALF(IUL), FAC)
          else
            FAC = ZERO
          end if
        else if(K.eq.2) then
C----     For passive transitions
          call DIVIDE   (YBPIJ(ITAU,JUL), ALF(IUL), FAC)
        else
C----     For optically thin and/or 2-photon transitions
          call PEN      (ITAU, KUP, JLO, SINGLE, XNU, Z, XINK, FINK,
     $                   FAC)
        end if
C
        TERM = AIJ(KUP,JLO)*PRAT*FAC
C
      else
        TERM = ZERO
      end if
C     !END
      call BYE ('MUSA')
C
      return
      end

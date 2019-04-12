      subroutine MESA
     $(ITAU,JUP,KLO,KRJ,RHOIJ,YBRIJ,AIJ,KIJ,LIJ,ALF,YBPIJ,SET,XNU,
     $ Z,XINK,FINK,TERM)
C
C     Rudolf Loeser, 1978 Nov 28
C---- Calculates the term "A*Rho".
C     !DASH
      save
C     !DASH
      real*8 AIJ, ALF, FINK, ONE, RAT, RHO, RHOIJ, SET, TERM, XINK, XNU,
     $       YBPIJ, YBRIJ, Z, ZERO
      integer ITAU, IUL, JUL, JUP, K, KIJ, KLO, KRJ, LIJ, N, NL
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external INDXUL, INTRANS, DIVIDE, SORGHUM, WING, HI, BYE
C
C               AIJ(NL,NL), KIJ(NL,NL), LIJ(MUL), XNU(NSL), SET(N,MUL),
      dimension AIJ(NL,*),  KIJ(NL,*),  LIJ(*),   XNU(*),   SET(*),
C
C               RHOIJ(N,NT), YBRIJ(N,NT), XINK(INK), Z(N), YBPIJ(N,NT),
     $          RHOIJ(N,*),  YBRIJ(N,*),  XINK(*),   Z(*), YBPIJ(N,*),
C
C               FINK(INK), ALF(MUL)
     $          FINK(*),   ALF(*)
C     !EJECT
C
      call HI ('MESA')
C     !BEG
      K = KIJ(JUP,KLO)
      if((K.ge.1).and.(K.le.4)) then
C
        call INDXUL     (JUP, KLO, IUL)
        call INTRANS    (JUP, KLO, 'MESA', JUL)
        call SORGHUM    (KRJ, LIJ(IUL), SINGLE)
C
        if(K.eq.1) then
C----     For radiative transitions
          if(SINGLE) then
            call DIVIDE (YBRIJ(ITAU,JUL), ALF(IUL), RAT)
            RHO = ONE+RAT
          else
            RHO = RHOIJ(ITAU,JUL)
          end if
        else if(K.eq.2) then
C----     For passive transitions
          call DIVIDE   (YBPIJ(ITAU,JUL), ALF(IUL), RAT)
          RHO = ONE+RAT
        else
C----     For 2-photon and thin transitions
          call WING     (ITAU, JUP, KLO, SINGLE, SET, XNU, Z, XINK,
     $                   FINK, N, NL, RHO)
        end if
C
        TERM = AIJ(JUP,KLO)*RHO
C
      else
        TERM = ZERO
      end if
C     !END
      call BYE ('MESA')
C
      return
      end

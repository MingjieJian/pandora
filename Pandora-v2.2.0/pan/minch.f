      subroutine MINCH
     $(E,NLEV,G,FJIN,FJINL,SFJIN,XQ,F,A,NMX,XQMAX,DQMIN,DQMAX,RACC,
     $ CUT,NND,DUMP)
C
C     Rudolf Loeser, 1984 Jul 09
C---- Computes the injection function for Hydrogen excitation,
C     FJIN(N,E), as a function of N, for a given value of E.
C
C---- DUMP is a switch for dump output;
C     NND is the index of that value of V for which a dump is wanted
C     (NND=0 means: no dump, NND=-1 means: for NLEV/2).
C     !DASH
      save
C     !DASH
      real*8 A, CUT, DQMAX, DQMIN, E, F, FJIN, FJINL, G, RACC, SFJIN,
     $       XN, XQ, XQMAX, ZERO
      integer N, NLEV, NMX, NND
      logical DMPN, DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DUNDAS, ANKER, MUDDLE, DANBUR, HI, BYE
C
C               G(NLEV), FJIN(NLEV), FJINL(NLEV), XQ(NMX), F(NMX),
      dimension G(*),    FJIN(*),    FJINL(*),    XQ(*),   F(*),
C
C               A(NMX)
     $          A(*)
C     !EJECT
C
      call HI ('MINCH')
C     !BEG
      if(DUMP) then
C----   Heading
        call DUNDAS (E)
      end if
C
      SFJIN = ZERO
      do 100 N = 2,NLEV
C----   Set up dump control
        call ANKER  (DUMP, NLEV, NND, N, DMPN)
C----   Compute FJIN, etc.
        XN = N
        call MUDDLE (XN, E, G(N), FJIN(N), FJINL(N), XQ, F, A, NMX,
     $               XQMAX, DQMIN, DQMAX, RACC, CUT, DMPN)
C
        SFJIN = SFJIN+FJIN(N)
  100 continue
C
      if(DUMP) then
        call DANBUR (NLEV, G, FJIN, FJINL, SFJIN)
      end if
C     !END
      call BYE ('MINCH')
C
      return
      end

      subroutine DEPART
     $(G,N,CHI,ELL,GPR,M,ALF,GAM,Z,THETA,H,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Sets up the values of G0 and SIGMA such that
C     Partition Function Q = G0 + SIGMA,
C     using the formulation and numerical tables of
C
C     Traving, G., Baschek, G., and Holweger, H.,
C     "Tabellen fuer die Berechnung der Zustandsummen,"
C     Abhandlungen von der Hamburger Sternwarte, VIII, 1, (1966).
C
C     Also returns Ionization Potential, X.
C     !DASH
      save
C     !DASH
      real*8 ALF, CHI, ELL, G, G0, GAM, GPR, H, HALF, SIGMA, T, TEN,
     $       THETA, WAS, X, Z, ZERO
      integer I, M, N, NU, NUE, NUS
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external QUASH, HI, BYE
C
C               CHI(N), ELL(N), GPR(N), M(N), ALF(M[i]), GAM(M[i])
      dimension CHI(*), ELL(*), GPR(*), M(*), ALF(*),    GAM(*)
C
      call HI ('DEPART')
C     !BEG
      X  = CHI(1)
      G0 = G
C
      SIGMA = ZERO
      if(H.gt.(ELL(1)+HALF)) then
        NUE = 0
        do 101 I = 1,N
          if(M(I).gt.0) then
            NUS = NUE+1
            NUE = NUE+M(I)
            do 100 NU = NUS,NUE
              T     = TEN**(-(GAM(NU)*THETA))
              SIGMA = SIGMA+ALF(NU)*T
  100       continue
          end if
C
          if(H.gt.ELL(I)) then
            call QUASH (ELL(I),Z,H,THETA,WAS)
            T     = TEN**(-(CHI(I)*THETA))
            SIGMA = SIGMA+GPR(I)*WAS*T
          end if
  101   continue
      end if
C     !END
      call BYE ('DEPART')
C
      return
      end

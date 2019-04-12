      subroutine CHIEF
     $(ITAU,IU,IL,N,NL,KRJ,GMI,RHOIJ,YBRIJ,BDI,X,IX,Z,PE,FE,PSZ,PSAR,
     $ FSZ,FSAR)
C
C     Rudolf Loeser, 1987 Dec 11
C---- Computes PE and FE at depth N, from upper level equations.
C     (This is version 3 of CHIEF.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, BDI, FE, FSAR, FSZ, GB, GMI, PE, PSAR, PSZ, RHOIJ, X,
     $       YBRIJ, Z, ZERO
      integer IL, ITAU, IU, IX, KRJ, L, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external EPEE, ARROW, HI, BYE
C
      dimension X(*), IX(*)
C
C               GMI(N,NSL), RHOIJ(N,NT), BDI(N,NL), YBRIJ(N,NT),
      dimension GMI(*),     RHOIJ(*),    BDI(*),    YBRIJ(*),
C
C               Z(NL,NL)
     $          Z(NL,*)
C
C     !EJECT
C
      call HI ('CHIEF')
C     !BEG
      PSZ  = ZERO
      PSAR = ZERO
      FSZ  = ZERO
      FSAR = ZERO
C
      do 100 L = 1,NL
        PSZ = PSZ+Z(IU,L)
        if(L.ne.IU) then
          call EPEE    (ITAU, L, IU, IL, GMI, BDI, N, GB)
          FSZ = FSZ+Z(L,IU)*GB
        end if
  100 continue
C
      if(IU.gt.1) then
        do 101 L = 1,(IU-1)
          if(L.ne.IL) then
            call ARROW (ITAU, IU, L, KRJ, RHOIJ, YBRIJ, X, IX, ARHO)
            PSAR = PSAR+ARHO
          end if
  101   continue
      end if
C
      if(IU.lt.NL) then
        do 102 L = (IU+1),NL
          call EPEE    (ITAU, L, IU, IL, GMI, BDI, N, GB)
          call ARROW   (ITAU, L, IU, KRJ, RHOIJ, YBRIJ, X, IX, ARHO)
          FSAR = FSAR+ARHO*GB
  102   continue
      end if
C
      FE = FSZ+FSAR
      PE = PSZ+PSAR
C     !END
      call BYE ('CHIEF')
C
      return
      end

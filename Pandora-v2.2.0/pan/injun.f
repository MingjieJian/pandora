      subroutine INJUN
     $(ITAU,IU,IL,N,NL,KRJ,GMI,RHOIJ,YBRIJ,BDI,X,IX,Z,PE,FE,PSZ,PSAR,
     $ FSZ,FSAR)
C
C     Rudolf Loeser, 1987 Dec 11
C---- Computes PE and FE at depth N, from the lower level equations.
C     (This is version 3 of INJUN.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, BDI, FE, FSAR, FSZ, GB, GMI, GRAT, PE, PSAR, PSZ,
     $       RHOIJ, X, YBRIJ, Z, ZERO
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
      external EPEE, ARROW, DIVIDE, HI, BYE
C
      dimension X(*), IX(*)
C
C               GMI(N,NSL), RHOIJ(N,NT), YBRIJ(N,NT), BDI(N,NL),
      dimension GMI(N,*),   RHOIJ(*),    YBRIJ(*),    BDI(*),
C
C               Z(NL,NL)
     $          Z(NL,*)
C     !EJECT
C
      call HI ('INJUN')
C     !BEG
      PSZ  = ZERO
      PSAR = ZERO
      FSZ  = ZERO
      FSAR = ZERO
C
      do 100 L = 1,NL
        FSZ = FSZ+Z(IL,L)
        if(L.ne.IL) then
          call EPEE    (ITAU, L, IU, IU, GMI, BDI, N, GB)
          PSZ = PSZ+Z(L,IL)*GB
        end if
  100 continue
C
      if(IL.gt.1) then
        do 101 L = 1,(IL-1)
          call ARROW   (ITAU, IL, L, KRJ, RHOIJ, YBRIJ, X, IX, ARHO)
          FSAR = FSAR+ARHO
  101   continue
      end if
C
      if(IL.lt.NL) then
        do 102 L = (IL+1),NL
          if(L.ne.IU) then
            call EPEE  (ITAU, L, IU, IU, GMI, BDI, N, GB)
            call ARROW (ITAU, L, IL, KRJ, RHOIJ, YBRIJ, X, IX, ARHO)
            PSAR = PSAR+ARHO*GB
          end if
  102   continue
      end if
C
      call DIVIDE      (GMI(ITAU,IL), GMI(ITAU,IU), GRAT)
C
      FE = (FSZ+FSAR)*GRAT
      PE = (PSZ+PSAR)
C     !END
      call BYE ('INJUN')
C
      return
      end

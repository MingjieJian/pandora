      subroutine COMPLEX
     $(IU,IL,IM,N,NL,KRJ,RHOIJ,YBRIJ,BDI,KIJ,WEIGHT,GMI,CIJ,PIJ,X,IX,
     $ PE,FE,Z,PSZ,PSAR,FSZ,FSAR,DUMP)
C
C     Rudolf Loeser, 1987 Dec 11
C---- COMPLEX sets up the statistical equilibrium equations
C     corresponding to (IM,IU,IL).
C     (This is version 3 of COMPLEX.)
C     !DASH
      save
C     !DASH
      real*8 BDI, CIJ, FE, FSAR, FSZ, GMI, PE, PIJ, PSAR, PSZ, RHOIJ,
     $       WEIGHT, X, YBRIJ, Z
      integer I, IL, IM, IU, IX, KIJ, KRJ, N, NL
      logical DUMP, UPPER
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, ZEA, CHIEF, INJUN, SOMMER, HI, BYE
C
      dimension X(*), IX(*)
C
C               WEIGHT(MUL,NT), GMI(N,NSL), FE(N), RHOIJ(N,NT), FSZ(N),
      dimension WEIGHT(*),      GMI(*),     FE(*), RHOIJ(*),    FSZ(*),
C
C               CIJ(N,NL**2), PIJ(N,NL**2), YBRIJ(N,NT), PSZ(N), PE(N),
     $          CIJ(*),       PIJ(*),       YBRIJ(*),    PSZ(*), PE(*),
C
C               KIJ(NL,NL), Z(NL,NL,N), PSAR(N), FSAR(N), BDI(N,NL)
     $          KIJ(*),     Z(NL,NL,*), PSAR(*), FSAR(*), BDI(*)
C     !EJECT
C
      call HI ('COMPLEX')
C     !BEG
      if((IM.ne.IU).and.(IM.ne.IL)) then
        write (MSSLIN(1),100) IU,IL,IM
  100   format('u =',I12,',l =',I12,', m =',I12,'; this combination ',
     $         'does not make sense.')
        call HALT    ('COMPLEX', 1)
      end if
C
      UPPER = IM.eq.IU
C
C---- Compute
      do 101 I = 1,N
        call ZEA     (I, IU, IL, N, NL, KRJ, CIJ, PIJ, GMI, BDI, RHOIJ,
     $                KIJ, WEIGHT, YBRIJ, X, IX, Z(1,1,I))
        if(UPPER) then
          call CHIEF (I, IU, IL, N, NL, KRJ, GMI, RHOIJ, YBRIJ, BDI,
     $                X, IX, Z(1,1,I), PE(I), FE(I), PSZ(I), PSAR(I),
     $                FSZ(I), FSAR(I))
        else
          call INJUN (I, IU, IL, N, NL, KRJ, GMI, RHOIJ, YBRIJ, BDI,
     $                X, IX, Z(1,1,I), PE(I), FE(I), PSZ(I), PSAR(I),
     $                FSZ(I), FSAR(I))
        end if
  101 continue
C
C---- Dump (if needed)
      call SOMMER    (DUMP, N, NL, IU, IL, IM, Z, PSZ, PSAR, FSZ, FSAR)
C     !END
      call BYE ('COMPLEX')
C
      return
      end

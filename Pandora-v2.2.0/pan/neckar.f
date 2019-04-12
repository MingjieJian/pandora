      subroutine NECKAR
     $(ITAU,N,NL,KRJ,X,IX,CIJ,RHOIJ,YBRIJ,GMI,CQUI,GVL,XM,DUMP)
C
C     Rudolf Loeser, 1988 Jul 28
C---- Computes the matrix M, for RHEIN.
C     (This is version 2 of NECKAR.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, AUGC, CIJ, CQUI, GMI, GVL, RHOIJ, TERM, X, XM, YBRIJ
      integer I, IL, ITAU, IX, J, JI, JL, JU, KRJ, L, LUEO, N, NL
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ARROW, CADDY, INDXIJ, ARROUT, HI, BYE
C
      dimension X(*), IX(*)
C
C               CIJ(N,NL**2), RHOIJ(N,NT), CQUI(N,NSL), YBRIJ(N,NT),
      dimension CIJ(N,*),     RHOIJ(*),    CQUI(N,*),   YBRIJ(*),
C
C               GVL(N,NL), GMI(N,NSL), XM(NL,NL)
     $          GVL(N,*),  GMI(N,*),   XM(NL,*)
C
      data JU,JL /0,0/
C     !EJECT
C
      call HI ('NECKAR')
C     !BEG
      do 103 J = 1,NL
        do 102 I = 1,NL
          if(I.eq.J) then
C----       Diagonal terms
            TERM = GVL(ITAU,J)
            if(I.gt.1) then
              do 100 L=1,I-1
                call ARROW  (ITAU, I, L, KRJ, RHOIJ, YBRIJ, X, IX,
     $                       ARHO)
                call INDXIJ (I, L, IL)
                TERM = TERM+ARHO+CIJ(ITAU,IL)
  100         continue
            end if
            if(I.lt.NL) then
              do 101 L = (I+1),NL
                call CADDY  (ITAU, I, L, KRJ, JU, JL, YBRIJ, X, IX,
     $                       AUGC)
                call INDXIJ (I, L, IL)
                TERM = TERM+CIJ(ITAU,IL)+AUGC
  101         continue
            end if
          else
C----       Off-diagonal terms
            call INDXIJ     (J, I, JI)
            TERM = CIJ(ITAU,JI)
            if(I.lt.J) then
              call ARROW    (ITAU, J, I, KRJ, RHOIJ, YBRIJ, X, IX,
     $                       ARHO)
              TERM = TERM+ARHO
            else
              call CADDY    (ITAU, J, I, KRJ, JU, JL, YBRIJ, X, IX,
     $                       AUGC)
              TERM = TERM+AUGC
            end if
          end if
C
          if(I.eq.J) then
            XM(I,J) =  GMI(ITAU,J)*TERM+CQUI(ITAU,J)
          else
            XM(I,J) = -GMI(ITAU,J)*TERM
          end if
  102   continue
  103 continue
      if(DUMP) then
        call ARROUT         (LUEO, XM, NL, NL, 'Matrix M')
      end if
C     !END
      call BYE ('NECKAR')
C
      return
      end

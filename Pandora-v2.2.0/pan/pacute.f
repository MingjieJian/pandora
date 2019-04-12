      subroutine PACUTE
     $(ITAU,N,NL,P,C,GM,CHI,SA,ASTAR,YBR,PIJ,CIJ,GMI,SAIJ,YIJ,YBRIJ)
C
C     Rudolf Loeser, 2003 Nov 19
C---- Extracts data at depth ITAU, for TEACUP/VAMOS.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, C, CHI, CIJ, GM, GMI, P, PIJ, SA, SAIJ, YBR, YBRIJ,
     $       YIJ, ZERO
      integer I, IN, ITAU, J, N, NL
      logical OK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXIJ, INDXNT, STYRAL, HI, BYE
C
C               P(N,NL**2), C(N,NL**2), GM(N,NSL), CHI(N,NT), SA(N,NT),
      dimension P(N,*),     C(N,*),     GM(N,*),   CHI(*),    SA(N,*),
C
C               ASTAR(N,NT), YBRIJ(NL,NL), PIJ(NL,NL), CIJ(NL,NL),
     $          ASTAR(N,*),  YBRIJ(NL,*),  PIJ(NL,*),  CIJ(NL,*),
C
C               SAIJ(NL,NL), YIJ(NL,NL), YBR(N,NT), GMI(NL)
     $          SAIJ(NL,*),  YIJ(NL,*),  YBR(N,*),  GMI(*)
C
      call HI ('PACUTE')
C     !BEG
      do 101 I = 1,NL
        GMI(I) = GM(ITAU,I)
        do 100 J = 1,NL
          call INDXIJ   (I, J, IN)
          CIJ(I,J) = C(ITAU,IN)
          PIJ(I,J) = P(ITAU,IN)
          SAIJ (I,J) = ZERO
          YBRIJ(I,J) = ZERO
          YIJ  (I,J) = ZERO
          if(I.gt.J) then
            call INDXNT (I, J, OK, IN)
            if(OK) then
              SAIJ (I,J) = SA (ITAU,IN)
              YBRIJ(I,J) = YBR(ITAU,IN)
            end if
            call STYRAL (I, J, ITAU, ASTAR, CHI, YIJ(I,J))
          end if
  100   continue
  101 continue
C     !END
      call BYE ('PACUTE')
C
      return
      end

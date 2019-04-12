      subroutine QUERY
     $(N,NL,M,FLM,CIJ,CQLI,CQL)
C
C     Rudolf Loeser, 1976 Sep 01
C---- Selects QL values for ALTAR.
C     !DASH
      save
C     !DASH
      real*8 CIJ, CQL, CQLI, FLM, ONE, Z, ZERO
      integer I, IMAX, IMIN, IQER, L, LIQIT, M, ML, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, INDXIJ, MINMAXD, DIVIDE, HI, BYE
C
C               FLM(N,NL), CIJ(N,NL**2), CQLI(N,NL), CQL(NL)
      dimension FLM(N,*),  CIJ(N,*),     CQLI(N,*),  CQL(*)
C
      data LIQIT /5/
C     !EJECT
C
      call HI ('QUERY')
C     !BEG
      do 103 L = 1,NL
        CQL(L) = ONE
        call INDXIJ      (M,L, ML)
C
        if(M.eq.L) then
          call ZERO1     (CQLI(1,L),N)
        else
          do 101 IQER = 1,LIQIT
            do 100 I = 1,N
              CQLI(I,L) = CIJ(I,ML)+CQL(L)*FLM(I,L)
  100       continue
            call MINMAXD (CQLI(1,L),1,N,IMIN,IMAX)
            if(CQLI(IMIN,L).gt.ZERO) then
              goto 102
            end if
            call DIVIDE  (CIJ(IMIN,ML),FLM(IMIN,L),Z)
            CQL(L) = -Z
  101     continue
          CQL(L) = ZERO
C
  102     continue
        end if
C
  103 continue
C     !END
      call BYE ('QUERY')
C
      return
      end

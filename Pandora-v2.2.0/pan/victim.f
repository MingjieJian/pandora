      subroutine VICTIM
     $(N,NL,M,CQL,BDI,CIJ,FLM,ARHO,CINC,XND,SUMS,SUMU)
C
C     Rudolf Loeser, 1976 Sep 01
C---- Computes intermediate terms for ALTAR -
C     SUMU (for EP2) and SUMS (for EP1).
C     (This is version 2 of VICTIM.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, BDI, CIJ, CINC, CQL, FLM, ONE, S, SUMS, SUMU, U, XND,
     $       ZERO
      integer I, L, M, ML, N, NL
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
      external INDXIJ, ZERO1, HI, BYE
C
C               FLM(N,NL), CQL(NL), BDI(N,NL), XND(N,NL), CIJ(N,NL**2),
      dimension FLM(N,*),  CQL(*),  BDI(N,*),  XND(N,*),  CIJ(N,*),
C
C               ARHO(N,NL), CINC(N,NL), SUMS(N), SUMU(N)
     $          ARHO(N,*),  CINC(N,*),  SUMS(*), SUMU(*)
C     !EJECT
C
      call HI ('VICTIM')
C     !BEG
      call ZERO1        (SUMS, N)
      call ZERO1        (SUMU, N)
C
      do 101 I = 1,N
        do 100 L = 1,NL
C
          if(L.ne.M) then
            if(L.lt.M) then
              S = ZERO
              U = ARHO(I,L)
            else
              S = CQL(L)*FLM(I,L)
              if(XND(I,M).eq.ZERO) then
                U = ZERO
              else
                U = -(ONE-CQL(L))*(XND(I,L)/XND(I,M))*ARHO(I,L)
              end if
            end if
            call INDXIJ (M, L, ML)
            SUMS(I) = SUMS(I)+BDI(I,L)*(CIJ(I,ML)+S)
            SUMU(I) = SUMU(I)+((CIJ(I,ML)+CINC(I,L))+U)
          end if
C
  100   continue
  101 continue
C     !END
      call BYE ('VICTIM')
C
      return
      end

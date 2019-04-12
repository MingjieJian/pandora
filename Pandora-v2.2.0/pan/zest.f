      subroutine ZEST
     $(ITAU,N,NL,P,TEX,DENS,XNE,XNU,XNUC,TER,CEI,AIJ,AATIJ,NPQ,LRQ,
     $ CMCE,CACE,SIJ)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Recomputes SIJ, part of CIJ, at depth ITAU.
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AIJ, CACE, CE, CEI, CMCE, DENS, EX, F, HNUKT, ONE,
     $       P, PR, SIJ, TER, TEX, XNE, XNU, XNUC
      integer I, IJ, ITAU, J, LRQ, N, NL, NPQ, NTE
      logical DUMP, KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external INDXIJ, ADELA, CEREAL, DIVIDE, MASHED, PROD, HI, BYE
C
C               SIJ(N,NL**2), XNU(NSL), TER(NTE), CEI(NTE,MUL), P(NSL),
      dimension SIJ(N,*),     XNU(*),   TER(*),   CEI(*),       P(*),
C
C               LRQ(NSL), CACE(MUL), AATIJ(NL,NL), NPQ(NSL), XNUC(NSL),
     $          LRQ(*),   CACE(*),   AATIJ(*),     NPQ(*),   XNUC(*),
C
C               CMCE(MUL), AIJ(NL,NL)
     $          CMCE(*),   AIJ(*)
C
      call HI ('ZEST')
C     !BEG
      KILROY = .true.
      do 101 J = 1,NL
        do 100 I = 1,NL
C
          call INDXIJ     (I, J, IJ)
          if(I.ne.J) then
C----       Set up for debug dump
            call ADELA    (KILROY, 'ZEST', ITAU, I, J, DUMP)
C----       Get CE
            call CEREAL   (I, J, TER, CEI, TEX, DENS, AIJ, AATIJ, XNU,
     $                     XNUC, P, NPQ, LRQ, CMCE, CACE, DUMP, CE)
C----       Common factor
            F = CE*XNE
C
            if(I.gt.J) then
C----         Term below the diagonal
              call DIVIDE (P(J), P(I), PR)
              SIJ(ITAU,IJ) = F*PR
            else
C----         Term above the diagonal
              call PROD   (TEX, (XNU(J)-XNU(I)), 1, HNUKT, EX)
              SIJ(ITAU,IJ) = F*EX
            end if
C
          else
            SIJ(ITAU,IJ) = ONE
          end if
C
  100   continue
  101 continue
      if(.not.KILROY) then
        call MASHED       ('ZEST')
      end if
C     !END
      call BYE ('ZEST')
C
      return
      end

      subroutine SAND
     $(N,NL,M,EP1,EP2,RS,CKI,PKL,PL1,PLK,P1L,CQI,BDI,GVL,KDGV,EPCBR,
     $ SPKL,PS1,AL)
C
C     Rudolf Loeser, 1976 Dec 02
C---- Computes EP1 and EP2 for PIE.
C     !DASH
      save
C     !DASH
      real*8 AL, BDI, CKI, CQI, DIV, EP1, EP2, EPCBR, FAC, FP1, FP2,
     $       GVL, ONE, P1L, PKL, PL1, PLK, PS1, R, RAT, RS, SPKL, TERM,
     $       ZERO
      integer I, KDGV, M, N, NL
      logical GTERM
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
      external DIVIDE, HI, BYE
C
C               EP1(N), EP2(N), RS(N), PS1(N), PKL(N), PL1(N), SPKL(N),
      dimension EP1(*), EP2(*), RS(*), PS1(*), PKL(*), PL1(*), SPKL(*),
C
C               P1L(N), CQI(N), BDI(N,NL), CKI(N,NSL), AL(NL), PLK(N),
     $          P1L(*), CQI(*), BDI(N,*),  CKI(N,*),   AL(*),  PLK(*),
C
C               GVL(N,NL)
     $          GVL(N,*)
C     !EJECT
C
      call HI ('SAND')
C     !BEG
      GTERM = (KDGV.gt.0)
C
      do 100 I = 1,N
C
        DIV = PL1(I)+PLK(I)+PS1(I)
        call DIVIDE ((PL1(I)+PS1(I)),DIV,RAT)
        FP1 = (PKL(I)+SPKL(I))*RAT
        call DIVIDE (PLK(I),DIV,FAC)
        FP2 = P1L(I)*FAC
        call DIVIDE (ONE,RS(I),R)
        call DIVIDE (FP1,BDI(I,M),RAT)
C
        if(GTERM) then
          TERM = GVL(I,M)
        else
          TERM = ZERO
        end if
C
        EP1(I) = (CKI(I,M)+(ONE-CQI(I))*FP1+AL(M)*FAC*SPKL(I))*R
        EP2(I) = (CKI(I,M)+FP2-CQI(I)*RAT+TERM)*R
  100 continue
C     !END
      call BYE ('SAND')
C
      return
      end

      subroutine PLUME
     $(N,TE,XNE,CHI,CPR,HND,RAB,ABDEL,
     $ INDX1,POP1K,POP1,INDX2,POP2K,POP2,KODI,X1,X2,D1,D2)
C
C     Rudolf Loeser, 1978 Sep 27
C---- Computes intermediates, for NOYON.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, CHI, CPR, D1, D2, HND, ONE, POP1, POP1K, POP2,
     $       POP2K, RAB, TE, TOTAL, X1, X2, XDEN, XNE
      integer I, INDX1, INDX2, KODI, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external VERA, DIVIDE, HI, BYE
C
C               CHI(NMT), XNE(N), HND(N), POP1K(N), POP2K(N), ABDEL(N),
      dimension CHI(*),   XNE(*), HND(*), POP1K(*), POP2K(*), ABDEL(*),
C
C               POP1(N,LIMP1), POP2(N,LIMP2), X1(N), X2(N), CPR(N,NMT),
     $          POP1(*),       POP2(*),       X1(*), X2(*), CPR(N,*),
C
C               D1(N), D2(N), RAB(N), TE(N)
     $          D1(*), D2(*), RAB(*), TE(*)
C
C
      call HI ('PLUME')
C     !BEG
      if(KODI.gt.0) then
C
        do 100 I = 1,N
          call VERA   (CPR(I,INDX1), CHI(INDX1), POP1K(I), POP1(I),
     $                 TE(I), XNE(I), X1(I))
          call VERA   (CPR(I,INDX2), CHI(INDX2), POP2K(I), POP2(I),
     $                 TE(I), XNE(I), X2(I))
          TOTAL = (ABDEL(I)*RAB(I))*HND(I)
          XDEN  = (ONE+X1(I)*(ONE+X2(I)))
          call DIVIDE (TOTAL, XDEN, D1(I))
          D2(I) = X1(I)*D1(I)
  100   continue
C
      end if
C     !END
      call BYE ('PLUME')
C
      return
      end

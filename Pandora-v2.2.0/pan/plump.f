      subroutine PLUMP
     $(N,TE,XNE,CHI,CPR,HND,RAB,ABDEL,INDX1,POP1K,POP1,
     $ INDX2,POP2K,POP2,INDX3,POP3K,POP3,KODI,X1,X2,X3,D1,D2,D3)
C
C     Rudolf Loeser, 2007 Jan 15
C---- Computes intermediates, for CALVIN.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, CHI, CPR, D1, D2, D3, HND, ONE, POP1, POP1K, POP2,
     $       POP2K, POP3, POP3K, RAB, TE, TOTAL, X1, X2, X3, XDEN, XNE
      integer I, INDX1, INDX2, INDX3, KODI, N
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
C               D1(N), D2(N), RAB(N), TE(N), POP3(N,LIMP3), POP3K(N),
     $          D1(*), D2(*), RAB(*), TE(*), POP3(*),       POP3K(*),
C
C               D3(N), X3(N)
     $          D3(*), X3(*)
C
C
      call HI ('PLUMP')
C     !BEG
      if(KODI.gt.0) then
        do 100 I = 1,N
          call VERA   (CPR(I,INDX1), CHI(INDX1), POP1K(I), POP1(I),
     $                 TE(I), XNE(I), X1(I))
          call VERA   (CPR(I,INDX2), CHI(INDX2), POP2K(I), POP2(I),
     $                 TE(I), XNE(I), X2(I))
          call VERA   (CPR(I,INDX3), CHI(INDX3), POP3K(I), POP3(I),
     $                 TE(I), XNE(I), X3(I))
          TOTAL = (ABDEL(I)*RAB(I))*HND(I)
          XDEN  = ONE+X1(I)*(ONE+X2(I)*(ONE+X3(I)))
          call DIVIDE (TOTAL, XDEN, D1(I))
          D2(I) = X1(I)*D1(I)
          D3(I) = X2(I)*D2(I)
  100   continue
      end if
C     !END
      call BYE ('PLUMP')
C
      return
      end

      subroutine SUDAN
     $(INDX,N,TE,XNE,CHI,CPR,HND,RAB,ABDEL,KODI,X,D)
C
C     Rudolf Loeser, 1978 Sep 27
C---- Computes intermediates, for NAMUR.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, CHI, CPR, D, HND, ONE, RAB, TE, X, XDEN, XNE, XNUM
      integer I, INDX, KODI, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external FLOCK, DIVIDE, HI, BYE
C
C               RAB(N), XNE(N), HND(N), X(N), D(N), CHI(NMT), ABDEL(N),
      dimension RAB(*), XNE(*), HND(*), X(*), D(*), CHI(*),   ABDEL(*),
C
C               CPR(N,NMT), TE(N)
     $          CPR(N,*),   TE(*)
C
      call HI ('SUDAN')
C     !BEG
      if(KODI.gt.0) then
        do 100 I = 1,N
          call FLOCK  (TE(I),XNE(I),CPR(I,INDX),CHI(INDX),X(I))
          XNUM = (ABDEL(I)*RAB(I))*HND(I)
          XDEN = (ONE+X(I))
          call DIVIDE (XNUM,XDEN,D(I))
  100   continue
      end if
C     !END
      call BYE ('SUDAN')
C
      return
      end

      subroutine BRASS
     $(KK,N,S,PA,PG,OMD,DLC,FNDT,GA,CP,IMG,FO,DMP1,XJBAR)
C
C     Rudolf Loeser, 2004 May 07
C---- Computes Jbar for the current transition, and also
C     returns intermediates GA and CP (CP needed for RHO).
C     (This is version 2 of BRASS.)
C     !DASH
      save
C     !DASH
      real*8 CP, DLC, FNDT, FO, GA, OMD, PA, PG, S, SUM, XJBAR, ZERO
      integer I, IMG, J, KK, N
      logical DMP1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ASGARD, LUGG, HI, BYE
C
C               PA(N,N), PG(N,N), XJBAR(N), S(N), GA(N), CP(N), IMG(N),
      dimension PA(N,*), PG(N,*), XJBAR(*), S(*), GA(*), CP(*), IMG(*),
C
C               OMD(N), DLC(N), FO(N), FNDT(N)
     $          OMD(*), DLC(*), FO(*), FNDT(*)
C
      call HI ('BRASS')
C     !BEG
      do 101 I = KK,N
        SUM = ZERO
        do 100 J = 1,N
          SUM = SUM+S(J)*PA(I,J)+PG(I,J)
  100   continue
        GA(I) = SUM
        CP(I) = DLC(I)+GA(I)+FNDT(I)
C
        XJBAR(I) = S(I)*OMD(I)+CP(I)
  101 continue
C
C---- Edit
      call ASGARD (N, KK, XJBAR, S, IMG, FO)
C
      if(DMP1) then
        call LUGG (KK, N, DLC, OMD, FNDT, S, GA, CP, XJBAR)
      end if
C     !END
      call BYE ('BRASS')
C
      return
      end

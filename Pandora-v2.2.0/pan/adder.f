      subroutine ADDER
     $(JLEV,ITAU,N,MTR,TR,XNU,CP,RK,TRI,RKR,RKI,TRMN,TRMX,SOME,DUMP)
C
C     Rudolf Loeser, 1984 Apr 16
C---- Gets starting values of TR, at depth ITAU.
C     (This is version 3 of ADDER.)
C     !DASH
      save
C     !DASH
      real*8 CP, ONE, RK, RKI, RKR, TR, TRI, TRMN, TRMX, XNU
      integer I, IA, IB, ITAU, JLEV, KNT, MTR, N
      logical DUMP, SOME
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  TATTLE, ELDONIA, CNIDOS, HOLD, EDESSA, CONSUB, MOVE1,
     $          HI, BYE
      intrinsic sign
C
C               RKI(MTR), TRI(MTR), TR(N,MTR), XNU(NSL), CP(NSL+1),
      dimension RKI(*),   TRI(*),   TR(*),     XNU(*),   CP(*),
C
C               RKR(MTR)
     $          RKR(*)
C     !EJECT
C
      call HI ('ADDER')
C     !BEG
C---- Extract suitable TR values
      call TATTLE     (TR, ITAU, N, MTR, TRI, KNT, SOME)
C
      if(SOME) then
C----   Compute corresponding RK's
        do 100 I = 1,KNT
          call CNIDOS (JLEV, XNU, CP, TRI(I), RKR(I))
  100   continue
C
C----   Compute differences w.r.t. target
        call MOVE1    (RKR, KNT, RKI)
        call CONSUB   (RK, RKI, KNT)
C
C----   Find trial interval
        call ELDONIA  (TRI, RKI, KNT, TRMN, TRMX)
        if(sign(ONE,RKI(KNT)).ne.sign(ONE,RKI(KNT-1))) then
C----     Refine interval further
          call EDESSA (KNT, RK, RKI, TRI, IA, IB, SOME, JLEV, XNU, CP,
     $                 TRMN, TRMX)
        end if
      end if
C
      if(DUMP) then
        call HOLD     (ITAU, SOME, RK, TRI, RKR, RKI, KNT, IA, IB, TRMN,
     $                 TRMX)
      end if
C     !END
      call BYE ('ADDER')
C
      return
      end

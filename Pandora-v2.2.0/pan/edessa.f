      subroutine EDESSA
     $(KNT,RK,RKI,TRI,IA,IB,SOME,JLEV,XNU,CP,TRMN,TRMX)
C
C     Rudolf Loeser, 1992 Nov 18
C---- Interval refinement for ADDER.
C     (This is version 2 of EDESSA.)
C     !DASH
      save
C     !DASH
      real*8 CP, DTR, FAC, ONE, RK, RKA, RKB, RKI, TEN, TRA, TRB, TRI,
     $       TRMN, TRMX, XNU, ZERO
      integer I, IA, IB, II, JLEV, KNT
      logical SOME
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external  CNIDOS, HI, BYE
      intrinsic sign, min, max
C
C               RKI(KNT), TRI(KNT), XNU(NSL), CP(NSL+1)
      dimension RKI(*),   TRI(*),   XNU(*),   CP(*)
C
C     !EJECT
C
      call HI ('EDESSA')
C     !BEG
      II = KNT
      if(RKI(II).le.ZERO) then
        IA = II
        IB = II-1
      else
        IA = II-1
        IB = II
      end if
C
      DTR = (TRI(IB)-TRI(IA))/TEN
      TRB = TRI(IA)
      RKB = RKI(IA)
C
      do 100 I = 1,10
        FAC = I
        TRA = TRB
        RKA = RKB
C
        if(I.eq.10) then
          TRB = TRI(IB)
          RKB = RKI(IB)
        else
          TRB = TRI(IA)+FAC*DTR
          call CNIDOS (JLEV,XNU,CP,TRB,RKB)
          RKB = RKB-RK
        end if
C
        if(sign(ONE,RKA).ne.sign(ONE,RKB)) then
          TRMN = min(TRA,TRB)
          TRMX = max(TRA,TRB)
          goto 101
        end if
  100 continue
      SOME = .false.
C
  101 continue
C     !END
      call BYE ('EDESSA')
C
      return
      end

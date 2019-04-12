      subroutine GONE
     $(KAMB,HND,HEND,XN1N,S,R,YRAT,N,MN1,G1,DIDG1)
C
C     Rudolf Loeser, 1988 Jul 21
C---- Computes GNV-1 (i.e. G1) for possible later use.
C     !DASH
      save
C     !DASH
      real*8 DELTA, G1, HEND, HND, R, RAT, ROT, S, XN1N, YRAT, ZERO
      integer I, IFLG, KAMB, MN1, N
      logical DIDG1
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
      external  ZERO1, DIVIDE, COMPD, HI, BYE
C
C               XN1N(N), HEND(N), HND(N), S(N), R(N), G1(N), YRAT(N)
      dimension XN1N(*), HEND(*), HND(*), S(*), R(*), G1(*), YRAT(*)
C
      data      DELTA /1.D-13/
C     !EJECT
C
      call HI ('GONE')
C     !BEG
      call ZERO1      (G1,N)
C
      do 100 I = 1,MN1
        call COMPD    (S(I),XN1N(I),DELTA,IFLG)
        if(IFLG.eq.0) then
          RAT = ONE
        else
          call DIVIDE (S(I),XN1N(I),RAT)
        end if
        RAT = RAT*YRAT(I)
C
        if(KAMB.eq.1) then
          call DIVIDE (R(I),HND(I) ,ROT)
        else
          call DIVIDE (R(I),HEND(I),ROT)
        end if
C
        call COMPD    (RAT,ROT,DELTA,IFLG)
        if(IFLG.eq.0) then
          G1(I) = ZERO
        else
          G1(I) = RAT-ROT
        end if
  100 continue
C
      DIDG1 = .true.
C     !END
      call BYE ('GONE')
C
      return
      end

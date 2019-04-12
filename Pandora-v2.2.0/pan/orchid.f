      subroutine ORCHID
     $(N,WEIGHT,RHOWT,XINCH,WMX,WMN,SMP,A,B,C)
C
C     Rudolf Loeser, 1974 Oct 29
C---- Uses the "new" method to establish a set of Rho weights, WEIGHT,
C     based on the previous set, RHOWT.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, ONE, RHOWT, SMP, TWO, WEIGHT, WMN, WMX, XINCH,
     $       ZERO
      integer I, KASE, KEQ, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  RANGED, MOVE1, POT, HALT, SHARD, HI, BYE
      intrinsic max, min
C
C               RHOWT(N), A(N), B(N), C(N), WEIGHT(N)
      dimension RHOWT(*), A(*), B(*), C(*), WEIGHT(*)
C     !EJECT
C
      call HI ('ORCHID')
C     !BEG
      call RANGED     (A,1,N,ZERO,ONE,KEQ)
      if(KEQ.eq.N) then
        call MOVE1    (RHOWT,N,WEIGHT)
      else
C
C----   Modify current weights, if necessary
        do 101 I = 1,N
          call POT    (KASE,A(I),B(I),C(I))
C
          if(KASE.eq.1) then
C           M
            RHOWT(I) = min(WMX,(RHOWT(I)+    XINCH))
C
          else if(KASE.eq.3) then
C           O
            RHOWT(I) = max(WMN,(RHOWT(I)-TWO*XINCH))
C
          else if(KASE.ne.2) then
            write (MSSLIN(1),100) I,KASE
  100       format('I =',I12,'; KASE =',I12,', which is neither 1, ',
     $             '2, nor 3.')
            call HALT ('ORCHID',1)
          end if
  101   continue
C
C----   and smooth them
        call SHARD    (WEIGHT,N,SMP,RHOWT)
C
      end if
C     !END
      call BYE ('ORCHID')
C
      return
      end

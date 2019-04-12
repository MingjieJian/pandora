      subroutine WRYBILL
     $(N,TOP,SIG,RULED,RULP,OK)
C
C     Rudolf Loeser, 1999 Nov 05
C---- Prepares RULED for plotting.
C     (This is version 2 of WRYBILL.)
C     !DASH
      save
C     !DASH
      real*8 ONE, RULED, RULP, SIG, TOP
      integer I, N
      logical OK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               RULED(N), RULP(N)
      dimension RULED(*), RULP(*)
C
      call HI ('WRYBILL')
C     !BEG
      OK = .false.
      do 100 I = 1,N
        if(RULED(I).eq.ONE) then
          RULP(I) = TOP
          OK = .true.
        else
          RULP(I) = SIG
        end if
  100 continue
C     !END
      call BYE ('WRYBILL')
C
      return
      end

      subroutine VEERY
     $(EP,CRIT,N,BAD)
C
C     Rudolf Loeser, 1984 Oct 24
C---- Checks to see whether EP is bad.
C     !DASH
      save
C     !DASH
      real*8 CRIT, EP
      integer I, N
      logical BAD
C     !DASH
      external HI, BYE
C
C               EP(N)
      dimension EP(*)
C
      call HI ('VEERY')
C     !BEG
      BAD = .false.
C
      do 100 I = 1,N
        if(EP(I).lt.CRIT) then
          BAD = .true.
          goto 101
        end if
  100 continue
C
  101 continue
C     !END
      call BYE ('VEERY')
C
      return
      end

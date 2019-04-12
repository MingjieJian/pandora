      subroutine EERIE
     $(LRQ,NSL,LM1,LM3)
C
C     Rudolf Loeser, 1992 Apr 06
C---- Sets flags for "ATOM" printout.
C     !DASH
      save
C     !DASH
      integer J, LRQ, NSL
      logical LM1, LM3
C     !DASH
      external HI, BYE
C
C               LRQ(NSL)
      dimension LRQ(*)
C
      call HI ('EERIE')
C     !BEG
      LM1 = .false.
      LM3 = .false.
C
      do 100 J = 1,NSL
        if(LRQ(J).eq.(-1)) then
          LM1 = .true.
        else if(LRQ(J).eq.(-3)) then
          LM3 = .true.
        end if
  100 continue
C     !END
      call BYE ('EERIE')
C
      return
      end

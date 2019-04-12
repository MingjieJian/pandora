      subroutine THRASH
     $(LU,NOLD,KOLD,GOOD)
C
C     Rudolf Loeser, 2002 Apr 01
C---- Reads arrays sizes for next restart JNU set.
C     (This is version 2 of THRASH.)
C     !DASH
      save
C     !DASH
      integer KOLD, LU, NOLD
      logical GOOD
      character LINE*8
C     !DASH
      external HI, BYE
C
      call HI ('THRASH')
C     !BEG
      GOOD = .false.
  100 continue
        read (LU,101,end=103) LINE
  101   format(A8)
        if( (LINE(:2).eq.'> ') .or. (LINE(:2).eq.'[ ') ) goto 100
C
      read (LINE,102) NOLD,KOLD
  102 format(2I4)
      GOOD = .true.
C
  103 continue
C     !END
      call BYE ('THRASH')
C
      return
      end

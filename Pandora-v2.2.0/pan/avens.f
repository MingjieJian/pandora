      subroutine AVENS
     $(JSTCN, KADD,KCMP,KCOP)
C
C     Rudolf Loeser, 1995 Apr 06
C---- Unpacks JSTCN.
C     (This is version 2 of AVENS.)
C     !DASH
      save
C     !DASH
      integer I, JSTCN, KADD, KCMP, KCOP
C     !DASH
      external  HI, BYE
      intrinsic mod
C
      call HI ('AVENS')
C     !BEG
      if(JSTCN.le.0) then
        KADD = 0
        KCMP = 0
        KCOP = 0
      else
C
        I    = JSTCN
        KADD = mod(I,2)
        I    = I/2
        KCMP = mod(I,2)
        I    = I/2
        KCOP = mod(I,2)
C
      end if
C     !END
      call BYE ('AVENS')
C
      return
      end

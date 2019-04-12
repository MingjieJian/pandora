      subroutine FLIPSD
     $(A,INC,N,NSC)
C     Rudolf Loeser, 1983 Oct 31
C---- Counts changes of sign.
C     !DASH
      save
C     !DASH
      real*8 A, ONE
      integer I, IN, INC, IP, N, NSC
C     !DASH
      intrinsic sign
C
      dimension A(*)
C
      data ONE /1.D0/
C
C     !BEG
      NSC = 0
      if(N.gt.1) then
        IN = 1
        do 100 I = 2,N
          IP = IN
          IN = IN+INC
          if(sign(ONE,A(IP)).ne.sign(ONE,A(IN))) then
            NSC = NSC+1
          end if
  100   continue
      end if
C     !END
C
      return
      end

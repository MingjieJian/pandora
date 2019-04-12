      subroutine KINGU
     $(M5,N3,W1,W2)
C
C     Rudolf Loeser, 1981 Oct 28
C---- Matrix manipulation for NIDABA.
C     W1(N+3,2N+5)  ->  W2(N+3,N+3).
C     !DASH
      save
C     !DASH
      real*8 W1, W2
      integer J, JL, JR, M5, N3
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external ARRADD, MOVE1, HALT, HI, BYE
C
C               W1(N3,M5), W2(N3,N3)
      dimension W1(N3,*),  W2(N3,*)
C
      call HI ('KINGU')
C     !BEG
      JL = 0
      JR = M5+1
      do 100 J = 1,(M5/2)
        JL = JL+1
        JR = JR-1
        call ARRADD (W1(1,JL),W1(1,JR),W2(1,J),N3)
  100 continue
C
      JL = JL+1
      JR = JR-1
      if(JL.ne.JR) then
        write (MSSLIN(1),101) JL,JR
  101   format('JL =',I12,', JR =',I12,'; they should be the same.')
        call HALT   ('KINGU',1)
      end if
C
      call MOVE1    (W1(1,JL),N3,W2(1,JL))
C     !END
      call BYE ('KINGU')
C
      return
      end

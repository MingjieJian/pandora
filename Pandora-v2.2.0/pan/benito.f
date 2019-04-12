      subroutine BENITO
     $(NO,WT,NW,N,A,LAB)
C
C     Rudolf Loeser, 1984 Jun 21
C---- Prints, for TWINK.
C     !DASH
      save
C     !DASH
      real*8 A, WT
      integer I, J, JE, JS, N, NO, NW
      character LAB*(*)
C     !DASH
      external  LINER, HI, BYE
      intrinsic min
C
C               WT(Nmkuse), A(N,Nmkuse)
      dimension WT(*),      A(N,*)
C
      call HI ('BENITO')
C     !BEG
      if(NO.gt.0) then
        call LINER   (4, NO)
        write (NO,100) LAB
  100   format(' ',A)
C
        JE = 0
  101   continue
          JS = JE+1
          JE = min((JE+12),NW)
          call LINER (2, NO)
          write (NO,102) (WT(J),J=JS,JE)
  102     format(' ',4X,1P12E10.2)
C
          call LINER (1, NO)
          do 104 I = 1,N
            write (NO,103) I,(A(I,J),J=JS,JE)
  103       format(' ',I4,1P12E10.2)
  104     continue
C
          if(JE.lt.NW) goto 101
        continue
C
      end if
C     !END
      call BYE ('BENITO')
C
      return
      end

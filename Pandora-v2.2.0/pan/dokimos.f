      subroutine DOKIMOS
     $(QT1,N1,QT2,N2,QT3,N3,QT4,N4,QALL,IPNT,LU)
C
C     Rudolf Loeser, 1991 Nov 25
C---- Prints a combined sorted list for POMP.
C     (IPNT is working storage.)
C     !DASH
      save
C     !DASH
      integer I, IPNT, L, LU, N, N1, N2, N3, N4
      character QALL*8, QT1*8, QT2*8, QT3*8, QT4*8
C     !DASH
      external MOVEC, SINGC, LINER, HI, BYE
C
C               QT1(N1), QT2(N2), QT3(N3), QT4(N4), QALL(N), IPNT(N)
      dimension QT1(*),  QT2(*),  QT3(*),  QT4(*),  QALL(*), IPNT(*)
C
      call HI ('DOKIMOS')
C     !BEG
      N = 0
C
      call MOVEC (QT1, 1, N1, QALL(N+1), 1, N1)
      N = N+N1
C
      call MOVEC (QT2, 1, N2, QALL(N+1), 1, N2)
      N = N+N2
C
      call MOVEC (QT3, 1, N3, QALL(N+1), 1, N3)
      N = N+N3
C
      call MOVEC (QT4, 1, N4, QALL(N+1), 1, N4)
      N = N+N4
C
      call SINGC (QALL, N, L, IPNT)
C
      call LINER (1, LU)
      write (LU,100) (QALL(I),I=1,N)
  100 format(' ','Trouble reading  PART H  input. List of valid ',
     $           'control fields:'//
     $      (' ',12A10))
C     !END
      call BYE ('DOKIMOS')
C
      return
      end

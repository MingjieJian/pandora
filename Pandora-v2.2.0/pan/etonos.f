      subroutine ETONOS
     $(QT1,N1, QT2,N2, QT3,N3, QT4,N4, QT5,N5,QT6,N6, QT7,N7, QT8,N8,
     $ QT9,N9, QT10,N10, QT11,N11, QALL,IPNT,LU)
C
C     Rudolf Loeser, 1991 Nov 25
C---- Prints a combined sorted list for VANILLA.
C     IPNT is working storage.
C     !DASH
      save
C     !DASH
      integer I, IPNT, L, LU, N, N1, N10, N11, N2, N3, N4, N5, N6, N7,
     $        N8, N9
      character QALL*8, QT1*8, QT10*8, QT11*8, QT2*8, QT3*8, QT4*8,
     $          QT5*8, QT6*8, QT7*8, QT8*8, QT9*8
C     !DASH
      external MOVEC, SINGC, LINER, HI, BYE
C
C               QT1(N1), QT2(N2), QT3(N3), QT4(N4), QT5(N5), QT11(N11),
      dimension QT1(*),  QT2(*),  QT3(*),  QT4(*),  QT5(*),  QT11(*),
C
C               QT6(N6), QT7(N7), QT8(N8), QT9(N9), QT10(N10), QALL(N),
     $          QT6(*),  QT7(*),  QT8(*),  QT9(*),  QT10(*),   QALL(*),
C
C               IPNT(N)
     $          IPNT(*)
C     !EJECT
C
      call HI ('ETONOS')
C     !BEG
      N = 0
      call MOVEC (QT1 ,1,N1 ,QALL(N+1),1,N1 )
      N = N+N1
      call MOVEC (QT2 ,1,N2 ,QALL(N+1),1,N2 )
      N = N+N2
      call MOVEC (QT3 ,1,N3 ,QALL(N+1),1,N3 )
      N = N+N3
      call MOVEC (QT4 ,1,N4 ,QALL(N+1),1,N4 )
      N = N+N4
      call MOVEC (QT5 ,1,N5 ,QALL(N+1),1,N5 )
      N = N+N5
      call MOVEC (QT6 ,1,N6 ,QALL(N+1),1,N6 )
      N = N+N6
      call MOVEC (QT7 ,1,N7 ,QALL(N+1),1,N7 )
      N = N+N7
      call MOVEC (QT8 ,1,N8 ,QALL(N+1),1,N8 )
      N = N+N8
      call MOVEC (QT9 ,1,N9 ,QALL(N+1),1,N9 )
      N = N+N9
      call MOVEC (QT10,1,N10,QALL(N+1),1,N10)
      N = N+N10
      call MOVEC (QT11,1,N11,QALL(N+1),1,N11)
      N = N+N11
C
      call SINGC (QALL,N,L,IPNT)
C
      call LINER (1,LU)
      write (LU,100) (QALL(I),I=1,N)
  100 format(' ','Trouble in VANILLA: reading  PART D  input.'//
     $       ' ','List of valid control fields:'//
     $      (' ',12A10))
C     !END
      call BYE ('ETONOS')
C
      return
      end

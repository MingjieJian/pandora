      subroutine SHRUG
     $(QT1,N1,QT2,N2,QT3,N3,QALL,IPNT,LUEO)
C
C     Rudolf Loeser, 1991 Nov 25
C---- Prints a combined sorted list for FENNEL.
C     (This is version 4 of SHRUG.)
C     !DASH
      save
C     !DASH
      integer I, IPNT, L, LUEO, N, N1, N2, N3
      character QALL*8, QT1*8, QT2*8, QT3*8
C     !DASH
      external MOVEC, SINGC, LINER, HI, BYE
C
C               QT1(N1), QT2(N2), QT3(N3), QALL(N), IPNT(N)
      dimension QT1(*),  QT2(*),  QT3(*),  QALL(*), IPNT(*)
C
      call HI ('SHRUG')
C     !BEG
      N = 0
      call MOVEC (QT1,1,N1,QALL(N+1),1,N1)
      N = N+N1
      call MOVEC (QT2,1,N2,QALL(N+1),1,N2)
      N = N+N2
      call MOVEC (QT3,1,N3,QALL(N+1),1,N3)
      N = N+N3
C
      call SINGC (QALL,N,L,IPNT)
C
      call LINER (1,LUEO)
      write (LUEO,100) (QALL(I),I=1,N)
  100 format(' ','Trouble reading  PART B  input.'//
     $       ' ','List of valid control fields:'//
     $      (' ',12A10))
C     !END
      call BYE ('SHRUG')
C
      return
      end

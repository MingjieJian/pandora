      subroutine LAUREL
     $(TAU,N,Y,F,W,IW,DUMP,TITLE,KODE)
C
C     Rudolf Loeser, 1968 May 28
C---- Computes the inverse-F matrix, for LOCUST.
C     Upon return, KODE=1 if calculation seems ok, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 F, TAU, W, Y
      integer I, IW, J, KODE, N
      logical DUMP
      character LABEL1*8, LABEL2*29, LABEL3*19, TITLE*(*)
C     !DASH
      external SEGMENT, IOTA, MOTOR, HI, BYE
C
      dimension W(*), IW(*)
C
C               TAU(N), F(N,N)
      dimension TAU(*), F(N,*)
C
      data LABEL1 /'F-matrix'/
      data LABEL2 /'F-matrix for QR weight matrix'/
      data LABEL3 /'Inverse of F-matrix'/
C
      call HI ('LAUREL')
C     !BEG
C---- Set up matrix, by rows
      do 101 I = 1,N
        do 100 J = 1,N
C----     Compute a single element
          call SEGMENT (J,N,Y,TAU(I),TAU(J),F(I,J))
  100   continue
  101 continue
C
      if(DUMP) then
        call IOTA      (F, N, TITLE, LABEL1)
      end if
C---- Invert
      call MOTOR       (F, N, LABEL2, W, IW, KODE)
      if((KODE.eq.1).and.DUMP) then
        call IOTA      (F, N, TITLE, LABEL3)
      end if
C     !END
      call BYE ('LAUREL')
C
      return
      end

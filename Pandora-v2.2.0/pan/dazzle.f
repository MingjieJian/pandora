      subroutine DAZZLE
     $(IB,IE,QAR,LINE,NO)
C
C     Rudolf Loeser, 1980 Dec 27
C---- Writes level designations.
C     (This is version 3 of DAZZLE.)
C     !DASH
      save
C     !DASH
      integer I, IB, IE, J, NO
      character BLANKS*8, LINE*120, QAR*10
C     !COM
C---- LEVDES      as of 1988 May 13
      integer     MILTY,LIMLV
      parameter   (MILTY=50)
C     (Remember to recompile ADAM when changing MILTY.)
      character   LEVDES*8
      dimension   LEVDES(MILTY)
      common      /LEVDES/ LIMLV,LEVDES
C
C     Term designations for levels of the ion of the run.
C     .
C     !DASH
      external RIGHT, FRUG, HI, BYE
C
C               QAR(16)
      dimension QAR(*)
C
      data BLANKS /'        '/
C
      call HI ('DAZZLE')
C     !BEG
      J = 1
      QAR(J) = BLANKS
C
      do 100 I = IB,IE
        J = J+1
        if(LEVDES(I).eq.BLANKS) then
          QAR(J) = BLANKS
        else
          call RIGHT (LEVDES(I),QAR(J),10)
        end if
  100 continue
C
      call FRUG      (BLANKS,QAR,J,LINE,NO)
C     !END
      call BYE ('DAZZLE')
C
      return
      end

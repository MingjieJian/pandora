      subroutine REVERSC
     $(A,INC,N,TEMPO)
C     Rudolf Loeser, 1996 Aug 21
C---- Reverses the order of elements of an array of the form:
C     A(1+0*INC), A(1+1*INC), A(1+2*INC), ... A(1+(N-1)*INC).
C     !DASH
      save
C     !DASH
      integer I, IH, IL, INC, LIM, N
      character A*(*), TEMPO*(*)
C     !DASH
      dimension A(*)
C
C     !BEG
      if(N.gt.1) then
        LIM = N/2
        IL = 1-INC
        IH = 1+N*INC
        do 100 I = 1,LIM
          IL = IL+INC
          IH = IH-INC
          TEMPO = A(IH)
          A(IH) = A(IL)
          A(IL) = TEMPO
  100   continue
      end if
C     !END
C
      return
      end

      subroutine BUNT
     $(LU,XVEC,LABEL)
C
C     Rudolf Loeser, 2002 Feb 05
C
C---- Writes XVEC, a depth-vector, in RESTART format in single
C     precision, to unit LU.
C
C     !DASH
      save
C     !DASH
      real*8 XVEC
      integer LU, MODE, N
      character LABEL*(*), MABEL*17
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external PUNT, HI, BYE
C
C               XVEC(N)
      dimension XVEC(*)
C
      data MODE /1/
C
      call HI ('BUNT')
C     !BEG
      if(LU.gt.0) then
        MABEL = LABEL
        call PUNT (LU,XVEC,N,MODE,MABEL)
      end if
C     !END
      call BYE ('BUNT')
C
      return
      end

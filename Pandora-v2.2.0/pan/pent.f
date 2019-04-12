      subroutine PENT
     $(LU,XVEC,IU,IL,LABEL)
C
C     Rudolf Loeser, 2002 Feb 05
C
C---- Writes XVEC, a depth-vector for transition (IU,IL), in single
C     precision, to unit LU.
C
C     !DASH
      save
C     !DASH
      real*8 XVEC
      integer IL, IU, LU, MODE, N
      character KABEL*11, LABEL*(*), MABEL*17
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
      call HI ('PENT')
C     !BEG
      if(LU.gt.0) then
        KABEL = LABEL
        write (MABEL,100) KABEL,IU,IL
  100   format(A11,2I3)
        call PUNT (LU,XVEC,N,MODE,MABEL)
      end if
C     !END
      call BYE ('PENT')
C
      return
      end

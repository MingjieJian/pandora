      subroutine PANT
     $(LU,XARR,N,M,MODE,LABEL)
C
C     Rudolf Loeser, 2002 Feb 05
C
C---- Writes XARR (N,M), an array of depth-vectors, in RESTART format,
C     to unit LU.
C
C     MODE = 1 means:  9 figures ("single" precision);
C     MODE = 2 means: 15 figures ("double" precision).
C     !DASH
      save
C     !DASH
      real*8 XARR
      integer J, LU, M, MODE, N
      character KABEL*11, LABEL*(*), MABEL*17
C     !DASH
      external PUNT, HI, BYE
C
C               XARR(N,M)
      dimension XARR(N,*)
C
      call HI ('PANT')
C     !BEG
      if(LU.gt.0) then
        KABEL = LABEL
        do 101 J = 1,M
          write (MABEL,100) KABEL,J
  100     format(A11,I3,3X)
          call PUNT (LU,XARR(1,J),N,MODE,MABEL)
  101   continue
      end if
C     !END
      call BYE ('PANT')
C
      return
      end

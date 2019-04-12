      subroutine TOAST
     $(CALLER)
C
C     Rudolf Loeser, 2005 Jan 26
C---- Checks X ?!
C     (This is version 2 of TOAST.)
C     !DASH
      save
C     !DASH
      real*8 AX, SIG, X, ZERO
      integer I, K, KMAX
      character CALLER*(*)
C     !COM
      common /TERRA/ X
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      intrinsic abs
C
      dimension X(1)
C
      data SIG  /1.D-308/
      data KMAX /1000/
C
C     !BEG
      K = 0
      do 101 I = 1,(IBSCR-1)
        AX = abs(X(I))
        if((AX.lt.SIG).and.(AX.ne.ZERO)) then
          write (*, 100) CALLER,I,X(I)
  100     format(' ','Toast: ',A,5X,I10,1PE24.16)
          K = K+1
          if(K.gt.KMAX) goto 102
        end if
  101 continue
  102 continue
      if(K.gt.0) stop 'Toast'
C     !END
C
      return
      end

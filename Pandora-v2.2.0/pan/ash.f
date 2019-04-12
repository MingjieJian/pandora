      subroutine ASH
     $(WN,N,TNU,IL)
C
C     Rudolf Loeser, 1968 Jun 04 (revised 2000 Jan 19)
C---- Constructs the elements in the lower right corner of the
C     expanded WN-matrix.
C     !DASH
      save
C     !DASH
      real*8 DH, DL, DU, TNU, TWTHRD, WN
      integer I, IL, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(17),TWTHRD)
C     !DASH
      external ZEROD, DIVIDE, HI, BYE
C
C               WN(N,N), TNU(N)
      dimension WN(N,*), TNU(*)
C
      call HI ('ASH')
C     !BEG
C---- Make sure that the two "discardable" rows =0
      call ZEROD    (WN(IL-2,1),N,N)
      call ZEROD    (WN(IL-1,1),N,N)
C
C---- Now insert approximate "tail"
      do 100 I = (IL-2),(N-1)
        DU = TNU(I+1)-TNU(I  )
        DH = TNU(I+1)-TNU(I-1)
        DL = TNU(I  )-TNU(I-1)
        call DIVIDE ( TWTHRD,(DL*DH),WN(I,I-1))
        call DIVIDE (-TWTHRD,(DU*DL),WN(I,I  ))
        call DIVIDE ( TWTHRD,(DU*DH),WN(I,I+1))
  100 continue
C
C---- Finally, make sure that the last row =0
      call ZEROD    (WN(N,1),N,N)
C     !END
      call BYE ('ASH')
C
      return
      end

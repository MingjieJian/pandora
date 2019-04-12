      subroutine THYME
     $(J,MODEL,M,A,N,QNAME,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Constructs a call to a vector reader for arrays with two indices.
C     !DASH
      save
C     !DASH
      real*8 A, W, Z, ZAUX
      integer J, LZA, M, MODEL, N
      character QNAME*8
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external BASIL, CREAM, LINER, HALT, HI, BYE
C
      dimension W(*)
C
C               A(N,M), LZA(NZA), ZAUX(N,max(LZA)), Z(N)
      dimension A(N,*), LZA(*),   ZAUX(*),          Z(*)
C
C
      call HI ('THYME')
C     !BEG
      if(MODEL.eq.1) then
        call BASIL (A(1,J),N,QNAME)
      else if(MODEL.eq.2) then
        call CREAM (A(1,J),QNAME,J,0,LZA,ZAUX,Z,W)
      else
        write (MSSLIN(1),100) MODEL
  100   format('MODEL =',I12,', which is not 1 or 2.')
        call HALT  ('THYME',1)
      end if
C     !END
      call BYE ('THYME')
C
      return
      end

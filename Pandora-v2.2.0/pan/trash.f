      subroutine TRASH
     $(A,N,CRIT)
C
C     Rudolf Loeser, 1996 Apr 11
C---- Edits "junk" out of an array.
C     !DASH
      save
C     !DASH
      real*8 A, CRIT, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
C               A(N)
      dimension A(*)
C
      call HI ('TRASH')
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          if(abs(A(I)).lt.CRIT) then
            A(I) = ZERO
          end if
  100   continue
      end if
C     !END
      call BYE ('TRASH')
C
      return
      end

      subroutine GURAN
     $(A,N,LAB,J)
C
C     Rudolf Loeser, 2003 Apr 16
C---- Dumps a matrix, for GENGHIS.
C     !DASH
      save
C     !DASH
      real*8 A
      integer J, LUEO, N
      character LAB*2, TITLE*15
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ARROUT, HI, BYE
C
C               A(N,N)
      dimension A(*)
C
      call HI ('GURAN')
C     !BEG
      write (TITLE,100) LAB,J
  100 format(A2,' for TMU(',I3,')')
      call ARROUT (LUEO, A, N, N, TITLE)
C     !END
      call BYE ('GURAN')
C
      return
      end

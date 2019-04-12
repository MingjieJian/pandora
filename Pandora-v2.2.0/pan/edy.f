      subroutine EDY
     $(Y,N,IMG,FO,ITER)
C
C     Rudolf Loeser, 1998 Jun 15
C---- Edits y, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 FO, Y, ZERO
      integer IMG, IOVER, ITER, KERM, N, NERM
      logical BAD
      character LABEL*90
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  EDITH, HI, BYE
C
C               Y(N), IMG(N), FO(N)
      dimension Y(*), IMG(*), FO(*)
C
      data      KERM, NERM /0, 1000/
C
      call HI ('EDY')
C     !BEG
      write (LABEL,100) ITER,IOVER
  100 format('y-editing; N1-iteration #',I3,', overall-iteration #',I4)
C
      call EDITH (Y,N, ZERO,2,2, 1,LABEL,IMG,FO,KERM,NERM, BAD)
C     !END
      call BYE ('EDY')
C
      return
      end

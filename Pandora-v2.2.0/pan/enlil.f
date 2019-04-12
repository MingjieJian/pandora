      subroutine ENLIL
     $(B,BK0,BK1,F,N,Y,H,B1)
C
C     Rudolf Loeser, 1992 Jan 03
C     SGK/Sep 22 2014 - do not call ABORT when beta => 0
C---- Prints an error message and aborts, for FROOT.
C     (This is version 4 of ENLIL.)
C     !DASH
      save
C     !DASH
      real*8 B, B1, BK0, BK1, F, H, Y, ZERO
      integer I, LUEO, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ABORT, MESHED, PHAROT, LINER, HI, BYE
C
C               B(N), BK0(N), BK1(N), F(N)
      dimension B(*), BK0(*), BK1(*), F(*)
C
      call HI ('ENLIL')
C     !BEG
      call MESHED ('ENLIL/DOMINO', 1)
      call PHAROT (LUEO)
C
      write (LUEO,100) Y,H
  100 format(' ','Unable to compute  beta1  for  y =',1PE16.8,5X,
     $           'H(y) =',E16.8//
     $       ' ',17X,'beta',14X,'K0',14X,'K1',15X,'F')
      call LINER  (1,LUEO)
      write (LUEO,101) (I,B(I),BK0(I),BK1(I),F(I),I=1,N)
  101 format(5(' ',I5,1P4E16.8/))
C
C --- do not call ABORT
C
      write (LUEO,102)
  102 format(' ','Continue with beta1 = 0')
      B1 = ZERO
C     !END
      call BYE ('ENLIL')
C
      return
      end

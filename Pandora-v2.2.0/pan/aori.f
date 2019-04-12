      subroutine AORI
     $(X,F,N,TAU,LAB,CALLER)
C
C     Rudolf Loeser, 1981 Nov 17
C---- Prints details of trapezoidal TAU integration.
C     (This is version 2 of AORI.)
C     !DASH
      save
C     !DASH
      real*8 F, TAU, X
      integer LUEO, N
      character CALLER*(*), LAB*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, VECOUT, MASHED, HI, BYE
C
C               X(N), F(N), TAU(N)
      dimension X(*), F(*), TAU(*)
C
      call HI ('AORI')
C     !BEG
      call MESHED (CALLER, 2)
C
      write (LUEO,100) LAB
  100 format(' ','Details of integration.'/
     $       ' ',A)
C
      call VECOUT (LUEO, X,   N, 'X'   )
      call VECOUT (LUEO, F,   N, 'OPAC')
      call VECOUT (LUEO, TAU, N, 'TAU' )
C
      call MASHED (CALLER)
C     !END
      call BYE ('AORI')
C
      return
      end

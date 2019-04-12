      subroutine LIONEL
     $(A,TAU,N,LABEL,AOLD,CALLER)
C
C     Rudolf Loeser, 2002 Apr 18
C---- Dumps, for SENTA.
C     (This is version 2 of LIONEL.)
C     !DASH
      save
C     !DASH
      real*8 A, AOLD, TAU
      integer LUEO, N
      character CALLER*(*), LABEL*(*)
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
C               A(N), TAU(N), AOLD(N)
      dimension A(*), TAU(*), AOLD(*)
C
      call HI ('LIONEL')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) LABEL
  100 format(' ','Editing negatives out of a function of TAU.'/
     $       ' ',A)
C
      call VECOUT (LUEO, TAU,  N, 'Tau')
      call VECOUT (LUEO, AOLD, N, 'Old')
      call VECOUT (LUEO, A,    N, 'New')
C
      call MASHED (CALLER)
C     !END
      call BYE ('LIONEL')
C
      return
      end

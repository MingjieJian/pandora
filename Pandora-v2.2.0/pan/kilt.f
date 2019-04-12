      subroutine KILT
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1982 May 11
C---- Allocates integer scratch storage for GROUND.
C     !DASH
      save
C     !DASH
      integer IN, IS, M, MUX, N, NLIM, NPNT, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 3),M  )
      equivalence (JZQ( 5),NT )
C
C---- ALVIN       as of 1995 Aug 08
      integer     LIMSCL
      parameter   (LIMSCL=11)
C     The number of TAU-scales to be printed fancily.
C     (Used in PICTURE, etc.)
C     .
C     !DASH
      external  IGET, ILCK, HI, BYE
      intrinsic min
C
      dimension IN(*)
C
      call HI ('KILT')
C     !BEG
      call IGET (IS ,CALLER)
C
      NLIM = min(NT,LIMSCL)
      NPNT = M+NLIM*(N-1)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NPNT
      IN( 3) = IN( 2)+NPNT
      MUX    = IN( 3)+NPNT
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('KILT')
C
      return
      end

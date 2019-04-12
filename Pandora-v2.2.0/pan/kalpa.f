      subroutine KALPA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Jul 24
C---- Allocates integer scratch storage for SAUCE.
C     (This is version 2 of KALPA.)
C     !DASH
      save
C     !DASH
      integer IN, INK, IS, MHM, MUX, N, NDT, NPNT, NSW, NWV
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(35),INK)
      equivalence (JZQ(22),MHM)
      equivalence (JZQ(17),NWV)
      equivalence (JZQ(21),NDT)
      equivalence (JZQ( 6),NSW)
C     !DASH
      external  IGET, ILCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('KALPA')
C     !BEG
      call IGET (IS ,CALLER)
C
      NPNT = max(N,INK,MHM,NWV,NDT,NSW)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NPNT
      MUX    = IN( 2)+NPNT
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('KALPA')
C
      return
      end

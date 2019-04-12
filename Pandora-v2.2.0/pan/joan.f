      subroutine JOAN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1974 Mar 22
C---- Allocates scratch storage for SWALLOW.
C     (This is version 3 of JOAN.)
C     !DASH
      save
C     !DASH
      integer IN, IS, ISN, KM, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
C
C---- KNTPEFE     as of 2003 Nov 20
      integer     KNTPF
      parameter   (KNTPF=5)
C     The number of alternative methods for computing PE and FE.
C     (Used in JOAN, SWALLOW.)
C     .
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JOAN')
C     !BEG
      call WGET (IS,  CALLER)
C
      ISN = N*KNTPF
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+ISN
      IN( 3) = IN( 2)+ISN
      IN( 4) = IN( 3)+ISN
      IN( 5) = IN( 4)+ISN
      IN( 6) = IN( 5)+ISN
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      MUX    = IN( 9)+N*KM
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JOAN')
C
      return
      end

      subroutine BUMF
     $(XND,XNK,OLD,IMG)
C
C     Rudolf Loeser, 2004 Mar 16
C---- Drives editing of "underflow", for NUMB.
C     !DASH
      save
C     !DASH
      real*8 OLD, XND, XNK
      integer IMG, N, NL
C     !COM
C---- ALTIMA      as of 2004 Mar 15
      real*8      ZZLALT,ZZSALT
      common      /ALTIMA/ ZZLALT,ZZSALT
C     Extreme values of NK, ND and BD.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external FOOZLE, HI, BYE
C
C               XND(N,NL), XNK(N), OLD(N), IMG(N)
      dimension XND(*),    XNK(*), OLD(*), IMG(*)
C
      call HI ('BUMF')
C     !BEG
      call FOOZLE (XND, XNK, NL, N, ZZSALT, OLD, IMG)
C     !END
      call BYE ('BUMF')
C
      return
      end

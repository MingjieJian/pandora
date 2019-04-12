      subroutine SPUD
     $(QNAME,RKWO,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 2002 Mar 12
C---- Reads obsolete RK-weights.
C     (This is version 2 of SPUD.)
C     !DASH
      save
C     !DASH
      real*8 RKWO, W, Z, ZAUX
      integer I, LUEO, LZA, N, NL
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MINT, CREAM, MESHED, MASHED, HI, BYE
C
      dimension W(*)
C
C               RKWO(N), ZAUX(LZM,NZM), LZA(50), Z(N)
      dimension RKWO(*), ZAUX(*),       LZA(*),  Z(*)
C
      call HI ('SPUD')
C     !BEG
      call MINT   (QNAME, I)
      call CREAM  (RKWO, QNAME, I, 0, LZA, ZAUX, Z, W)
C
      call MESHED ('SPUD', 3)
      write (LUEO,100) I
  100 format(' ','RKW ',I2,' is an obsolete input quantity and ',
     $           'should not be used.'/
     $       ' ','The corresponding current quantity is RKWT; ',
     $           'it should = (1 - RKW).')
      call MASHED ('SPUD')
C     !END
      call BYE ('SPUD')
C
      return
      end

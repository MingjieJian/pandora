      subroutine SPUN
     $(QNAME,RHWO,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 2002 Mar 12
C---- Reads obsolete RHO-weights.
C     (This is version 2 of SPUN.)
C     !DASH
      save
C     !DASH
      real*8 RHWO, W, Z, ZAUX
      integer IL, IU, IUL, LUEO, LZA, N
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MINT, INTRANS, CREAM, MESHED, MASHED, HI, BYE
C
      dimension W(*)
C
C               RHWO(N,NT), ZAUX(LZM,NZM), LZA(50), Z(N)
      dimension RHWO(N,*),  ZAUX(*),       LZA(*),  Z(*)
C
      call HI ('SPUN')
C     !BEG
      call MINT    (QNAME, IU)
      call MINT    (QNAME, IL)
      call INTRANS (IU, IL, 'SPUN', IUL)
      call CREAM   (RHWO(1,IUL), QNAME, IU, IL, LZA, ZAUX, Z, W)
C
      call MESHED  ('SPUN', 3)
      write (LUEO,100) IU,IL
  100 format(' ','RHOWT ',I2,'/',I2,' is an obsolete input quantity ',
     $           'and should not be used.'/
     $       ' ','The corresponding current quantity is RHWT; ',
     $           'it should = (1 - RHOWT).')
      call MASHED  ('SPUN')
C     !END
      call BYE ('SPUN')
C
      return
      end

      subroutine WEIRDO
     $(KDB,CALLER,KODE)
C
C     Rudolf Loeser, 2005 Dec 08
C---- Increments and tests Diana/Orion data blocks count.
C     (This is version 2 of WEIRDO.)
C     !DASH
      save
C     !DASH
      integer KDB, KM, KODE, KOUNT, LG, LUEO, MRR, NSHL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
      equivalence (JZQ(15),MRR)
      equivalence (JZQ(34),LG )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 4),NSHL )
C
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  MESHED, ABORT, HI, BYE
      intrinsic min
C
      call HI ('WEIRDO')
C     !BEG
      if(KODE.eq.0) then
        if(KDB.eq.0) then
          KOUNT = 0
        end if
        KOUNT = KOUNT+1
C
        KDB   = min(KOUNT,LEMUR)
C
      else
C
        if(KOUNT.gt.LEMUR) then
          call MESHED (CALLER, 1)
          write (LUEO,100) KOUNT,LEMUR,KM,LG,NSHL,MRR
  100     format(' ','KOUNT =',I8,', LEMUR (limit) =',I8,'; too many ',
     $               'Diana/Orion data blocks.'/
     $           ' ','Check # of frequencies, rays, and directions:'/
     $           ' ','  KM =',I12/
     $           ' ','  LG =',I12/
     $           ' ','NSHL =',I12/
     $           ' ',' MRR =',I12)
          call ABORT
        end if
C
      end if
C     !END
      call BYE ('WEIRDO')
C
      return
      end

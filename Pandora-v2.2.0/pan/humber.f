      subroutine HUMBER
     $(X,IX)
C
C     Rudolf Loeser, 1997 Jun 13
C---- Reads and massages parts A & B of input file, so that permanent
C     data storage can be allocated.
C     (This is version 2 of HUMBER.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IX, LUEO, LUIN
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 1),LUIN )
      equivalence (LUNITS( 6),LUEO )
C
C---- BERTH       as of 1990 Nov 20
      integer     LSHF,LADR,ISHF
      dimension   ISHF(7)
      common      /BERTH/ LSHF,LADR,ISHF
C     "Part-1 to Part-2" input shuffling data block.
C     (Allocated by GRUB.)
C     !DASH
      external MEEK, LUCK, GRUB, DORE, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('HUMBER')
C     !BEG
C---- Initialize execution performance statistics
      call MEEK
C---- Initialize main input file
      call LUCK (LUIN, LUEO)
      rewind LUIN
C---- Initialize "Part-1 to Part-2" input-shuffling data block
      call GRUB (ISHF, LSHF)
C
C---- Read first parts of input, and initialize
      call DORE (X, IX)
C     !END
      call BYE ('HUMBER')
C
      return
      end

      subroutine MAGOG
     $(KODE,TITLE)
C
C     Rudolf Loeser, 2003 Oct 01
C---- Error message and abort, for GAG.
C     !DASH
      save
C     !DASH
      integer KODE, LUEO
      character TITLE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ABORT, HI, BYE
C
      call HI ('MAGOG')
C     !BEG
      call MESHED ('MAGOG/GAG', 1)
      write (LUEO,100) KODE,TITLE
  100 format(' ','Determinant calculation failed.',10X,'KODE =',I12/
     $       ' ',A//
     $       ' ','The NOVA/VAMOS methods cannot be used for this ',
     $           'transition; choose another (METSE = 3 ?).'/
     $       ' ','(Make sure that option METSW is off.)')
      call ABORT
C     !END
      call BYE ('MAGOG')
C
      return
      end

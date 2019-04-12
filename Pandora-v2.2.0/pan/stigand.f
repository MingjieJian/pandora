      subroutine STIGAND
     $(IRAY, CALLER)
C
C     Rudolf Loeser, 1983 Sep 08
C---- Produces a dump heading, for FECAMP.
C     !DASH
      save
C     !DASH
      integer IRAY, LUEO
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('STIGAND')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) IRAY
  100 format(' ','Dump for shifted SNU: Shell Ray tangent to',I5,
     $           '. radius.')
C     !END
      call BYE ('STIGAND')
C
      return
      end

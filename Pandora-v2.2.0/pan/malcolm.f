      subroutine MALCOLM
     $(EMOO,CALLER)
C
C     Rudolf Loeser, 1983 Sep 08
C---- Produces a dump heading, for BOSHAM.
C     !DASH
      save
C     !DASH
      real*8 EMOO
      integer LUEO
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
      call HI ('MALCOLM')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) EMOO
  100 format(' ','Dump for shifted SNU: Mu =',F6.3,' in plane-',
     $           'parallel medium')
C     !END
      call BYE ('MALCOLM')
C
      return
      end

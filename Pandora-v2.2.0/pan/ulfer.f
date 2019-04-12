      subroutine ULFER
     $(FRR,CALLER)
C
C     Rudolf Loeser, 1983 Sep 08
C---- Produces a dump header, for SENLAC.
C     !DASH
      save
C     !DASH
      real*8 FRR
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
      call HI ('ULFER')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) FRR
  100 format(' ','Dump for shifted Snu: Disk Ray to FRR =',F10.7)
C     !END
      call BYE ('ULFER')
C
      return
      end

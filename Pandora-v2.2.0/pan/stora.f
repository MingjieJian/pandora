      subroutine STORA
     $(I,TE,J,BD,HNUKT,SE,RES)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Dump for H-bf emission.
C     !DASH
      save
C     !DASH
      real*8 BD, HNUKT, RES, SE, TE
      integer I, J, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('STORA')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) I,TE,J,BD,HNUKT,SE,RES
  100 format(' ',3X,'i',13X,'TE',2X,'j',13X,'BD',10X,'HNUKT',
     $           13X,'SE',45X,12X,'RES'/
     $       ' ',I4,1PE15.7,I3,3E15.7,45X,E15.7)
C     !END
      call BYE ('STORA')
C
      return
      end

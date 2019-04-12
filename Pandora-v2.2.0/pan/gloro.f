      subroutine GLORO
     $(I,ORES,OREM)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Dump for H-bf absorption.
C     !DASH
      save
C     !DASH
      real*8 OREM, ORES
      integer I, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external HI, BYE
C
      call HI ('GLORO')
C     !BEG
      write (LUEO,100) I,ORES,OREM
  100 format(' ',I4,3X,90X,1P2E15.7)
C     !END
      call BYE ('GLORO')
C
      return
      end

      subroutine DIDDLE
     $(TRY,RB,R,NTRY,LAB)
C
C     Rudolf Loeser, 1987 Feb 23
C---- Dumps, for SINEW.
C     (This is version 5 of DIDDLE.)
C     !DASH
      save
C     !DASH
      real*8 R, RB, TRY
      integer LUEO, NTRY
      character LAB*5
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
      call HI ('DIDDLE')
C     !BEG
      write (LUEO,100) RB,TRY,R,LAB,NTRY
  100 format(' ',30X,'RB=',1PE14.6,4X,'T=',E14.6,4X,'R=',E14.6,4X,
     $           A5,'=',I3)
C     !END
      call BYE ('DIDDLE')
C
      return
      end

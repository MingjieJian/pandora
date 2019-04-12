      subroutine ZINJA
     $(STKFN,XLR,XLTR,EMM,RES,IFALL,CALL,FALL,FACT,Z,DR,TERM,RAYS,
     $ OPAC,SCAT)
C
C     Rudolf Loeser, 2002 Sep 20
C---- Debug printout for BIE.
C     (This is version 2 of ZINJA.)
C     !DASH
      save
C     !DASH
      real*8 CALL, DR, EMM, FACT, FALL, OPAC, RAYS, RES, SCAT, STKFN,
     $       TERM, XLR, XLTR, Z
      integer IFALL, LUEO
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
      call HI ('ZINJA')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) XLR,XLTR,EMM,RES,FALL,CALL,IFALL,STKFN,FACT,Z,DR,
     $                 TERM,RAYS,OPAC,SCAT
  100 format(' ','wing',10X,'LR**2 =',1PE15.8,5X,'LTR =',E15.8,5X,
     $           'M =',E15.7,5X,'res. br. =',E15.7/
     $       ' ',14X,'FALL =',E15.7,5X,'CALL =',E15.7,1X,I1,5X,
     $           'STKFN =',E15.7/
     $       ' ',14X,'FACT =',E15.8,5X,'Z =',E12.4,5X,'DR =',E16.8/
     $       ' ',14X,'TERM =',E15.7,5X,'RAYS =',E15.7,5X,'OPAC =',
     $           E15.8,5X,'SCAT =',E15.8)
C     !END
      call BYE ('ZINJA')
C
      return
      end

      subroutine BLOB
     $(K,XL,NE,WN,GP,DMPW)
C
C     Rudolf Loeser, 1982 Mar 30
C---- Dumps, for DURIAN.
C     !DASH
      save
C     !DASH
      real*8 GP, WN, XL
      integer K, LUEO, NE
      logical DMPW
      character LINE*127, MESS*56
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, LABFIL, ARROUT, HI, BYE
C
C               WN(NE,NE), GP(NE,NE)
      dimension WN(*),     GP(*)
C
      call HI ('BLOB')
C     !BEG
      call LINER    (2, LUEO)
      write (MESS,100) K,XL
  100 format(' ','Lyman WN details: K =',I5,', XL =',1PE14.6)
      call LABFIL   (MESS, LINE)
      write (LUEO,101) LINE
  101 format(' ',A)
C
      if(DMPW) then
        call ARROUT (LUEO, WN, NE, NE ,'Wnbar')
      end if
      call ARROUT   (LUEO, GP, NE, NE ,'GP'   )
C     !END
      call BYE ('BLOB')
C
      return
      end

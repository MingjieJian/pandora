      subroutine RANTER
     $(DUMP,CALLER,LINE,KAR,JLEV,RNU,MRP)
C
C     Rudolf Loeser, 2002 Jan 17
C---- Final dump, for MANUEL.
C     !DASH
      save
C     !DASH
      real*8 RNU
      integer I, JLEV, KAR, LUEO, MRP
      logical DUMP
      character CALLER*(*), LINE*120
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MASHED, LINER, HI, BYE
C
C               RNU(MRX+1)
      dimension RNU(*)
C
      call HI ('RANTER')
C     !BEG
      if(DUMP) then
        if(KAR.gt.0) then
          write (LUEO,100) LINE
  100     format(' ',A120)
        end if
C
        call LINER  (1, LUEO)
        write (LUEO,101) JLEV
  101   format(' ','Level',I3,5X,'Finish')
        write (LUEO,102) (I,RNU(I),I=1,MRP)
  102   format(' ',I8,F12.6,I8,F12.6,I8,F12.6,I8,F12.6,I8,F12.6,
     $             I8,F12.6)
C
        call MASHED (CALLER)
      end if
C     !END
      call BYE ('RANTER')
C
      return
      end

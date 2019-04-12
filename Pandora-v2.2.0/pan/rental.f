      subroutine RENTAL
     $(DUMP,LINE,KAR,KNS,KIN,MRP)
C
C     Rudolf Loeser, 2002 Jan 17
C---- Dumps, for MANUEL.
C     !DASH
      save
C     !DASH
      integer KAR, KIN, KNS, LUEO, MRP
      logical DUMP
      character BLANK*1, LINE*120
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
      call HI ('RENTAL')
C     !BEG
      if(DUMP) then
        write (LINE(KAR+1:KAR+20),100) KNS,KIN,MRP
  100   format(5X,3I5)
        KAR = KAR+20
C
        if(KAR.eq.120) then
          write (LUEO,101) LINE
  101     format(' ',A120)
C
          KAR  = 0
          LINE = BLANK
        end if
      end if
C     !END
      call BYE ('RENTAL')
C
      return
      end

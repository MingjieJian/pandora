      subroutine TISRIT
     $(MUX,YY,MYX,LINE)
C
C     Rudolf Loeser, 1981 Jan 15
C---- Encodes intensity integral contribution data.
C     !DASH
      save
C     !DASH
      real*8 F99, HNDRDTH, ONE, YY
      integer MUX, MYX, N
      character LINE*13, TEXT*5
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      data F99,HNDRDTH /9.9D-1, 1.D-2/
C     !EJECT
C
      call HI ('TISRIT')
C     !BEG
      LINE= '(           )'
C
      if(YY.ge.ONE) then
        LINE( 2: 4) = '1.0'
      else if(YY.ge.F99) then
        LINE( 2: 4) = '.99'
      else if(YY.lt.HNDRDTH) then
        LINE( 2: 4) = '.00'
      else
        write (TEXT,100) YY
  100   format(F5.2)
        LINE( 2: 4) = TEXT(3:5)
      end if
C
      if((MUX.gt.0).and.(MUX.le.N).and.(MUX.le.999)) then
        write (LINE( 6: 8),101) MUX
  101   format(I3)
      else
        LINE( 7: 8) = '--'
      end if
C
      if((MYX.gt.0).and.(MYX.le.N).and.(MYX.le.999)) then
        write (LINE(10:12),101) MYX
      else
        LINE(10:12) = '--'
      end if
C     !END
      call BYE ('TISRIT')
C
      return
      end

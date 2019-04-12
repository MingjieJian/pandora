      subroutine SERLO
     $(CWS,ITS,KNW)
C
C     Rudolf Loeser, 2003 Oct 27
C---- Prints CWJ-adjustments summary.
C     (This is version 2 of SERLO.)
C     !DASH
      save
C     !DASH
      real*8 CWS
      integer I, ITS, KNW, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, LINER, HI, BYE
C
C               CWS(KNW), ITS(KNW)
      dimension CWS(*),   ITS(*)
C
      call HI ('SERLO')
C     !BEG
      call MESHED ('MILAN', 3)
      write (LUEO,100)
  100 format(' ','The following CWJ values were used ---'//
     $       ' ',6X,'RHO-iter.',12X,'CWJ')
      call LINER  (1, LUEO)
      write (LUEO,101) (I,ITS(I),CWS(I),I=1,KNW)
  101 format(' ',I5,I9,1PE16.6)
      call MASHED ('MILAN')
C     !END
      call BYE ('SERLO')
C
      return
      end

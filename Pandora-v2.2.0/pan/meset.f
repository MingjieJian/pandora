      subroutine MESET
     $(NO,TOT,TITLE)
C
C     Rudolf Loeser, 1991 Jul 16
C---- Prints a heading for eclipse profile data
C     !DASH
      save
C     !DASH
      integer NO
      character TITLE*5, TOT*24
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MESET')
C     !BEG
      write (NO,100) TOT
  100 format(' ',8X,'--------------------------------------------',
     $           10X,A)
      write (NO,101) TITLE
  101 format(' ',A5,3X,'Cumulative Flux Profiles, in ergs/cm**2/s/Hz')
      call LINER (1,NO)
      if(TITLE(1:1).eq.ALPHS(19)) then
        write (NO,102) 'FS','outer'
  102   format(' ',8X,A2,'(J) is the flux within the radius located ',
     $             'halfway between J and J-1 (to ',A,' maximum)')
      else
        write (NO,102) 'FD','Disk'
      end if
C     !END
      call BYE ('MESET')
C
      return
      end

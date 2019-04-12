      subroutine SMUDGE
     $(NO,IPOS,LINE)
C
C     Rudolf Loeser, 1978 May 28
C---- Prints error messages pertaining to calculation of Continuous
C     Emergent Intensity.
C     (This is version 2 of SMUDGE.)
C     !DASH
      save
C     !DASH
      integer IPOS, JSPC, NO
      character BLANK*1, LINE*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- NICNAC      as of 1988 Feb 10
      integer     KFS,KFN
      dimension   KFN(3)
      common      /NICNAC/ KFS,KFN
C     Error counts, for Continuous Emergent Intensity printouts.
C     .
C     !DASH
      external LINER, HI, BYE
C     !EJECT
C
      call HI ('SMUDGE')
C     !BEG
      if((NO.gt.0).and.(KFS.gt.0)) then
C
        LINE = BLANK
        JSPC = 1
C
        if(KFN(1).gt.0) then
          LINE(IPOS:) = '$ fewer than 4 points for integration'
          call LINER (JSPC,NO)
          write (NO,100) LINE
  100     format(' ',A)
          JSPC = 0
        end if
C
        if(KFN(2).gt.0) then
          LINE(IPOS:) = '* maximum TMU is less than 2'
          call LINER (JSPC,NO)
          write (NO,100) LINE
          JSPC = 0
        end if
C
        if(KFN(3).gt.0) then
          LINE(IPOS:) = '% integral blew up'
          call LINER (JSPC,NO)
          write (NO,100) LINE
        end if
C
      end if
C     !END
      call BYE ('SMUDGE')
C
      return
      end

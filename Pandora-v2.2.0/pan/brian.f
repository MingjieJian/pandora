      subroutine BRIAN
     $(KIND,QNAME,MAUX,LZA,ZAUX)
C
C     Rudolf Loeser, 1981 Apr 28
C---- Handles miscellaneous input, for POMP.
C     (This is version 2 of BRIAN.)
C     !DASH
      save
C     !DASH
      real*8 ZAUX
      integer KIND, KINDLIM, LZA, MAUX
      character QNAME*8
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external WITCH, DAHLIA, CHIRON, CENTAUR, CYAN, HALT, HI, BYE
C
C               ZAUX(NZM,LZM), LZA(50)
      dimension ZAUX(*),       LZA(*)
C
      data KINDLIM /7/
C
      call HI ('BRIAN')
C     !BEG
      if(KIND.eq.1) then
C----   Read auxiliary Z-table index
        call WITCH  (QNAME,MAUX)
      else if(KIND.eq.2) then
C----   Read auxiliary Z-table
        call DAHLIA (QNAME,LZA,ZAUX)
      else if(KIND.eq.3) then
C----   Switch input files
        call CHIRON
      else if((KIND.eq.4).or.(KIND.eq.6).or.(KIND.eq.7)) then
C----   Amend default Ion Data Table
        call CYAN   (QNAME)
      else if(KIND.eq.5) then
C----   Read filespec of "general" input file
        call CENTAUR
      else
        write (MSSLIN(1),100) KIND,KINDLIM
  100   format('KIND =',I12,', which is not between 1 and',I3,
     $         ', inclusive.')
        call HALT   ('BRIAN',1)
      end if
C     !END
      call BYE ('BRIAN')
C
      return
      end

      subroutine POPE
     $(CHI,FABD,PRTLM,IQPPF,IQUVP,LINE,NO)
C
C     Rudolf Loeser, 1980 Feb 14
C---- Prints table of elements data.
C     (This is version 2 of POPE.)
C     !DASH
      save
C     !DASH
      real*8 CHI, FABD, ONE, PRTLM
      integer IQPPF, IQUVP, NO
      character LINE*120
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ELEFANT, PRIAM, LINER, EGRET, HI, BYE
C
C               CHI(NMT)
      dimension CHI(*)
C     !EJECT
C
      call HI ('POPE')
C     !BEG
      if(NO.gt.0) then
        call PRIAM   (NO, 'ELEMENTS', 8)
        call LINER   (1, NO)
        write (NO,100)
  100   format(' ','Table of elements data')
        call LINER   (2, NO)
        call EGRET   (LINE, NO)
        if(FABD.ne.ONE) then
          call LINER (1,NO)
          write (NO,101) FABD
  101     format(' ','(All abundances but those of H and He have been ',
     $               'multiplied by input parameter FABD =',1PE10.3,')')
        end if
        if(IQUVP.gt.0) then
          call LINER (2, NO)
          write (NO,102) PRTLM
  102     format(' ','Partition function limit factor PARTLIM =',
     $                1PE10.3)
          if(IQPPF.le.0) then
            write (NO,103)
  103       format(' ','To see values, set PARTPRNT = ON.')
          end if
        end if
C
        call LINER   (2, NO)
        call ELEFANT (CHI, NO)
      end if
C     !END
      call BYE ('POPE')
C
      return
      end

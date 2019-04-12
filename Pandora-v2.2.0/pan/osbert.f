      subroutine OSBERT
     $(A,PHC,GMAI,XJNU,PHRAT,DOR,DUMP)
C
C     Rudolf Loeser, 1993 Aug 30
C---- Dumps, for JAFFA.
C     !DASH
      save
C     !DASH
      real*8 A, DOR, GMAI, PHC, PHRAT, XJNU
      integer LUEO
      logical DUMP
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
      call HI ('OSBERT')
C     !BEG
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) A,PHC,GMAI,XJNU,PHRAT,DOR
  100   format(' ',30X,1P,
     $             3X,'     A=',E12.4,2X,'PHI(KP)=',E12.4,
     $             3X,'   GMA=',E12.4,3X, '   JNU=',E12.4/
     $         ' ',30X,
     $             3X,'PHIRAT=',E12.4,3X,'   DOR=',E12.4)
      end if
C     !END
      call BYE ('OSBERT')
C
      return
      end

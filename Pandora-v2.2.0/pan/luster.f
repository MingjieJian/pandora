      subroutine LUSTER
     $(NO,IPRDD,IPRDF)
C
C     Rudolf Loeser, 2005 Jan 27
C---- Prints PRD printout control information.
C     (This is version 5 of LUSTER.)
C     !DASH
      save
C     !DASH
      integer IPRDD, IPRDF, NO
C     !DASH
      external LINER, HI, BYE
C
      call HI ('LUSTER')
C     !BEG
      if(NO.gt.0) then
        call LINER (1,NO)
        write (NO,100) IPRDF,IPRDD
  100   format(' ','Results for frequency XI = 0 are printed, and ',
     $             'for every IPRDF-th both ways from there;'/
     $         ' ','results at the first depth are printed, and ',
     $             'at every IPRDD-th in from there.'/
     $         ' ','In this run, IPRDF =',I5,' and IPRDD =',I5,'.')
      end if
C     !END
      call BYE ('LUSTER')
C
      return
      end

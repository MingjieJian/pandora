      subroutine TACKIT
     $(N,NV,NH,PLOT)
C
C     Rudolf Loeser, 1997 Aug 21
C---- Sets up Check graph.
C     !DASH
      save
C     !DASH
      integer LUEO, N, NC, NH, NV
      logical PLOT
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('TACKIT')
C     !BEG
      NH = 117
      NV = 2*N-1
      NC = NH*NV
C
      PLOT = NC.le.IMALEN
C
      if(.not.PLOT) then
        call MESHED ('TACKIT', 3)
        write (LUEO,100) N,NH,NV,NC,IMALEN
  100   format(' ','The CHECK-graphs are too large and cannot be done ',
     $             '(N is too large).'//
     $         ' ','N =',I12,5X,'NH =',I6,5X,'NV =',I6,5X,'NC =',I10,
     $             5X,'IMALEN =',I10)
      end if
C     !END
      call BYE ('TACKIT')
C
      return
      end

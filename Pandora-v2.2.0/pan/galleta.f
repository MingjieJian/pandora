      subroutine GALLETA
     $(NO,IJECT)
C
C     Rudolf Loeser, 1978 Jun 30
C---- Writes a heading, for RAJA.
C     !DASH
      save
C     !DASH
      integer IJECT, NO
C     !DASH
      external DEJECT, LINER, DOUBLER, HI, BYE
C
      call HI ('GALLETA')
C     !BEG
      if(NO.gt.0) then
        call DEJECT  (NO,IJECT)
C
        write (NO,100)
  100   format(' ',51X,'"ECLIPSE" Continuum Flux.')
        call LINER   (1,NO)
        call DOUBLER (NO)
        call LINER   (1,NO)
C
        write (NO,101)
  101   format(T10,'Calculated using spherical coordinates.')
        call LINER   (2,NO)
C
        write (NO,102)
  102   format(T10,'Z   = Height, in km'/
     $         T10,'K   = Opacity, /cm'/
     $         T10,'S   = Source function'/
     $         T10,'I   = Intensity, in erg/cm**2/sec/sterad/Hz'/
     $         T10,'E   = Energy per cm strip, ',
     $                   'in erg/cm/sec/sterad/Hz'/
     $         T10,'F   = Flux contributions, in erg/sec/sterad/Hz'/
     $         T10,'SH  = Shell flux, in erg/sec/sterad/Hz'/
     $         T10,'FRR = Radius fraction'/
     $         T10,'DRP = Limits of annuli, in km'/
     $         T10,'DR  = Mid-radii of annuli, in km'/
     $         T10,'A   = Integration weights'/
     $         T10,'DSK = Disk flux, in ergs/sec/sterad/Hz')
      end if
C     !END
      call BYE ('GALLETA')
C
      return
      end

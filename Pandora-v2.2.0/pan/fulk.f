      subroutine FULK
     $(NO,LUG,LABEL)
C
C     Rudolf Loeser, 1994 Nov 03
C---- Writes a legend for absorber/emitter printouts.
C     !DASH
      save
C     !DASH
      integer LUG, NO
      character LABEL*(*)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('FULK')
C     !BEG
      if(NO.gt.0) then
        call LINER (1,NO)
        if(LUG.gt.0) then
          write (NO,100) LABEL
  100     format(' ',' The letter appearing next to the ',A,' name ',
     $               'identifies that component in the plot below.')
        end if
        write (NO,101) LABEL
  101   format(' ',' The number "1" next to the ',A,' name means that ',
     $             'this component has been newly recomputed;'/
     $         ' ',' the number "0" means, instead, that it has not ',
     $             'been necessary to update the previously-',
     $             'computed value.')
      end if
C     !END
      call BYE ('FULK')
C
      return
      end
